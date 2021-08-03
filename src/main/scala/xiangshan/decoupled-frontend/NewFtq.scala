/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.frontend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils.{AsyncDataModuleTemplate, CircularQueuePtr, DataModuleTemplate, HasCircularQueuePtrHelper, SRAMTemplate, SyncDataModuleTemplate, XSDebug, XSError, XSPerfAccumulate}
import xiangshan._

import scala.tools.nsc.doc.model.Val
import utils.{ParallelPriorityEncoder, ParallelPriorityMux}
import xiangshan.backend.CtrlToFtqIO

class FtqPtr(implicit p: Parameters) extends CircularQueuePtr[FtqPtr](
  p => p(XSCoreParamsKey).FtqSize
){
  override def cloneType = (new FtqPtr).asInstanceOf[this.type]
}

object FtqPtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): FtqPtr = {
    val ptr = Wire(new FtqPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}

class FtqNRSRAM[T <: Data](gen: T, numRead: Int)(implicit p: Parameters) extends XSModule {

  val io = IO(new Bundle() {
    val raddr = Input(Vec(numRead, UInt(log2Up(FtqSize).W)))
    val ren = Input(Vec(numRead, Bool()))
    val rdata = Output(Vec(numRead, gen))
    val waddr = Input(UInt(log2Up(FtqSize).W))
    val wen = Input(Bool())
    val wdata = Input(gen)
  })

  for(i <- 0 until numRead){
    val sram = Module(new SRAMTemplate(gen, FtqSize))
    sram.io.r.req.valid := io.ren(i)
    sram.io.r.req.bits.setIdx := io.raddr(i)
    io.rdata(i) := sram.io.r.resp.data(0)
    sram.io.w.req.valid := io.wen
    sram.io.w.req.bits.setIdx := io.waddr
    sram.io.w.req.bits.data := VecInit(io.wdata)
  }

}

class Ftq_RF_Components(implicit p: Parameters) extends XSBundle with BPUUtils {
  val startAddr = UInt(VAddrBits.W)
  val nextRangeAddr = UInt(VAddrBits.W)
  val pftAddr = UInt(log2Ceil(PredictWidth).W)
  val isNextMask = Vec(PredictWidth, Bool())
  val oversize = Bool()
  val carry = Bool()
  def getPc(offset: UInt) = {
    def getHigher(pc: UInt) = pc(VAddrBits-1, log2Ceil(PredictWidth)+instOffsetBits)
    def getOffset(pc: UInt) = pc(log2Ceil(PredictWidth)+instOffsetBits-1, instOffsetBits)
    Cat(getHigher(Mux(isNextMask(offset), nextRangeAddr, startAddr)),
        getOffset(startAddr)+offset, 0.U(instOffsetBits.W))
  }
  def getFallThrough() = {
    getFallThroughAddr(this.startAddr, this.carry, this.pftAddr)
  }
}

class Ftq_pd_Entry(implicit p: Parameters) extends XSBundle {
  val brMask = Vec(PredictWidth, Bool())
  val jmpInfo = ValidUndirectioned(Vec(3, Bool()))
  val jmpOffset = UInt(log2Ceil(PredictWidth).W)
  val jalTarget = UInt(VAddrBits.W)
  val rvcMask = Vec(PredictWidth, Bool())
  def hasJal  = jmpInfo.valid && !jmpInfo.bits(0)
  def hasJalr = jmpInfo.valid && jmpInfo.bits(0)
  def hasCall = jmpInfo.valid && jmpInfo.bits(1)
  def hasRet  = jmpInfo.valid && jmpInfo.bits(2)

  def fromPdWb(pdWb: PredecodeWritebackBundle) = {
    val pds = pdWb.pd
    this.brMask := VecInit(pds.map(pd => pd.isBr && pd.valid))
    this.jmpInfo.valid := VecInit(pds.map(pd => (pd.isJal || pd.isJalr) && pd.valid)).asUInt.orR
    this.jmpInfo.bits := ParallelPriorityMux(pds.map(pd => (pd.isJal || pd.isJalr) && pd.valid),
                                             pds.map(pd => VecInit(pd.isJalr, pd.isCall, pd.isRet)))
    this.jmpOffset := ParallelPriorityEncoder(pds.map(pd => (pd.isJal || pd.isJalr) && pd.valid))
    this.rvcMask := VecInit(pds.map(pd => pd.isRVC))
    this.jalTarget := pdWb.jalTarget
  }

  def toPd(offset: UInt) = {
    require(offset.getWidth == log2Ceil(PredictWidth))
    val pd = Wire(new PreDecodeInfo)
    pd.valid := true.B
    pd.isRVC := rvcMask(offset)
    val isBr = brMask(offset)
    val isJalr = offset === jmpOffset && jmpInfo.valid && jmpInfo.bits(0)
    pd.brType := Cat(offset === jmpOffset && jmpInfo.valid, isJalr || isBr)
    pd.isCall := offset === jmpOffset && jmpInfo.valid && jmpInfo.bits(1)
    pd.isRet  := offset === jmpOffset && jmpInfo.valid && jmpInfo.bits(2)
    pd
  }
}

class Ftq_Redirect_SRAMEntry(implicit p: Parameters) extends XSBundle with HasBPUConst {
  val rasSp = UInt(log2Ceil(RasSize).W)
  val rasEntry = new RASEntry
  val specCnt = Vec(numBr, UInt(10.W))
}

class Ftq_1R_SRAMEntry(implicit p: Parameters) extends XSBundle with HasBPUConst {
  val meta = UInt(MaxMetaLength.W)
}

class Ftq_Pred_Info(implicit p: Parameters) extends XSBundle {
  val target = UInt(VAddrBits.W)
  val cfiIndex = ValidUndirectioned(UInt(log2Ceil(PredictWidth).W))
}

class FtqEntry(implicit p: Parameters) extends XSBundle with HasBPUConst {
  val startAddr = UInt(VAddrBits.W)
  val fallThruAddr = UInt(VAddrBits.W)
  val isNextMask = Vec(PredictWidth, Bool())

  val meta = UInt(MaxMetaLength.W)

  val rasSp = UInt(log2Ceil(RasSize).W)
  val rasEntry = new RASEntry
  val hist = new GlobalHistory
  val specCnt = Vec(numBr, UInt(10.W))
  
  val valids = Vec(PredictWidth, Bool())
  val brMask = Vec(PredictWidth, Bool())
  // isJalr, isCall, isRet
  val jmpInfo = ValidUndirectioned(Vec(3, Bool()))
  val jmpOffset = UInt(log2Ceil(PredictWidth).W)
  
  val mispredVec = Vec(PredictWidth, Bool())
  val cfiIndex = ValidUndirectioned(UInt(log2Ceil(PredictWidth).W))
  val target = UInt(VAddrBits.W)
}

class FtqRead[T <: Data](private val gen: T)(implicit p: Parameters) extends XSBundle {
  val ptr = Output(new FtqPtr)
  val offset = Output(UInt(log2Ceil(PredictWidth).W))
  val data = Input(gen)
  def apply(ptr: FtqPtr, offset: UInt) = {
    this.ptr := ptr
    this.offset := offset
    this.data
  }
  override def cloneType = (new FtqRead(gen)).asInstanceOf[this.type]
}


class FtqToBpuIO(implicit p: Parameters) extends XSBundle {
  val redirect = Valid(new BranchPredictionRedirect)
  val update = Valid(new BranchPredictionUpdate)
}

class FtqToBpuIOWrapper(implicit p: Parameters) extends XSBundle {
  val redirect = Valid(new BranchPredictionRedirectWrapper)
  val update = Valid(new BranchPredictionUpdateWrapper)
}

class FtqToIfuIO(implicit p: Parameters) extends XSBundle {
  val req = Decoupled(new FetchRequestBundle)
  val redirect = Valid(new Redirect)
}

trait HasBackendRedirectInfo extends HasXSParameter {
  def numRedirect = exuParameters.JmpCnt + exuParameters.AluCnt + 1
  def isLoadReplay(r: Valid[Redirect]) = r.bits.flushItself()
}

class FtqToCtrlIO(implicit p: Parameters) extends XSBundle with HasBackendRedirectInfo {
  val pc_reads = Vec(1 + numRedirect + 1 + 1, Flipped(new FtqRead(UInt(VAddrBits.W))))
  val target_read = Flipped(new FtqRead(UInt(VAddrBits.W)))
  def getJumpPcRead = pc_reads.head
  def getRedirectPcRead = VecInit(pc_reads.tail.dropRight(2))
  def getMemPredPcRead = pc_reads.init.last
  def getRoqFlushPcRead = pc_reads.last
}


class FTBEntryGen(implicit p: Parameters) extends XSModule with HasBackendRedirectInfo with HasBPUParameter {
  val io = IO(new Bundle {
    val start_addr = Input(UInt(VAddrBits.W))
    val old_entry = Input(new FTBEntry)
    val pd = Input(new Ftq_pd_Entry)
    val cfiIndex = Flipped(Valid(UInt(log2Ceil(PredictWidth).W)))
    val target = Input(UInt(VAddrBits.W))
    val hit = Input(Bool())
    val mispredict_vec = Input(Vec(PredictWidth, Bool()))

    val new_entry = Output(new FTBEntry)
    val new_br_insert_pos = Output(Vec(numBr, Bool()))
    val taken_mask = Output(Vec(numBr+1, Bool()))
    val mispred_mask = Output(Vec(numBr+1, Bool()))

    // for perf counters
    val is_init_entry = Output(Bool())
    val is_old_entry = Output(Bool())
    val is_new_br = Output(Bool())
    val is_jalr_target_modified = Output(Bool())
    val is_br_full = Output(Bool())
  })

  // no mispredictions detected at predecode
  val hit = io.hit
  val pd = io.pd

  val init_entry = WireInit(0.U.asTypeOf(new FTBEntry))


  val cfi_is_br = pd.brMask(io.cfiIndex.bits) && io.cfiIndex.valid
  val entry_has_jmp = pd.jmpInfo.valid
  val new_jmp_is_jal  = entry_has_jmp && !pd.jmpInfo.bits(0) && io.cfiIndex.valid
  val new_jmp_is_jalr = entry_has_jmp &&  pd.jmpInfo.bits(0) && io.cfiIndex.valid
  val new_jmp_is_call = entry_has_jmp &&  pd.jmpInfo.bits(1) && io.cfiIndex.valid
  val new_jmp_is_ret  = entry_has_jmp &&  pd.jmpInfo.bits(2) && io.cfiIndex.valid
  val last_jmp_rvi = entry_has_jmp && pd.jmpOffset === (PredictWidth-1).U && !pd.rvcMask.last
  val last_br_rvi = cfi_is_br && io.cfiIndex.bits === (PredictWidth-1).U && !pd.rvcMask.last
  
  val cfi_is_jal = io.cfiIndex.bits === pd.jmpOffset && new_jmp_is_jal
  val cfi_is_jalr = io.cfiIndex.bits === pd.jmpOffset && new_jmp_is_jalr

  def getLower(pc: UInt) = pc(log2Ceil(PredictWidth)+instOffsetBits-1, instOffsetBits)
  // if not hit, establish a new entry
  init_entry.valid := true.B
  // tag is left for ftb to assign
  init_entry.brValids(0) := cfi_is_br
  init_entry.brOffset(0) := io.cfiIndex.bits
  init_entry.brTargets(0) := io.target
  init_entry.jmpOffset := pd.jmpOffset
  init_entry.jmpValid := new_jmp_is_jal || new_jmp_is_jalr
  init_entry.jmpTarget := Mux(!cfi_is_jal, pd.jalTarget, io.target)
  val jmpPft = getLower(io.start_addr) +& pd.jmpOffset +& Mux(pd.rvcMask(pd.jmpOffset), 1.U, 2.U)
  init_entry.pftAddr := Mux(entry_has_jmp, jmpPft, getLower(io.start_addr) + Mux(last_br_rvi, 1.U, 0.U))
  init_entry.carry   := Mux(entry_has_jmp, jmpPft(log2Ceil(PredictWidth)).asBool, true.B)
  init_entry.isJalr := new_jmp_is_jalr
  init_entry.isCall := new_jmp_is_call
  init_entry.isRet  := new_jmp_is_ret
  init_entry.last_is_rvc := Mux(entry_has_jmp, pd.rvcMask(pd.jmpOffset), pd.rvcMask.last)

  init_entry.oversize := last_br_rvi || last_jmp_rvi

  // if hit, check whether a new cfi(only br is possible) is detected
  val oe = io.old_entry
  val br_recorded_vec = VecInit((oe.brValids zip oe.brOffset).map {
    case (v, off) => v && (off === io.cfiIndex.bits)
  })
  val br_recorded = br_recorded_vec.asUInt.orR
  val is_new_br = cfi_is_br && !br_recorded
  val br_full = oe.brValids.asUInt.andR // all slots have brs
  val new_br_offset = io.cfiIndex.bits
  // vec(i) means new br will be inserted BEFORE old br(i)
  val new_br_insert_onehot = VecInit((0 until numBr).map{
    i => i match {
      case 0 => oe.brValids(0) && new_br_offset < oe.brOffset(0)
      case idx => oe.brValids(idx-1) && new_br_offset > oe.brOffset(idx-1) &&
        (!oe.brValids(idx) || new_br_offset < oe.brOffset(idx))
    }
  })

  val old_entry_modified = WireInit(io.old_entry)
  for (i <- 0 until numBr) {
    old_entry_modified.brOffset(i)  :=  Mux(new_br_insert_onehot(i), new_br_offset,
                                          Mux(oe.brOffset(i) < new_br_offset, oe.brOffset(i),
                                            (if (i != 0) oe.brOffset(i-1) else oe.brOffset(i))))
    old_entry_modified.brTargets(i) :=  Mux(new_br_insert_onehot(i), io.target,
                                          Mux(oe.brOffset(i) < new_br_offset, oe.brTargets(i),
                                            (if (i != 0) oe.brTargets(i-1) else oe.brTargets(i))))
  }
  old_entry_modified.brValids := VecInit((oe.brValids zip new_br_insert_onehot).map{case (v1, v2) => v1 || v2})

  // in this case, pft_addr should be the addrs of the last br in packet
  val pft_need_to_change = is_new_br && br_full
  // it should either be the given last br or the new br
  when (pft_need_to_change) {
    val new_pft_offset = Mux(new_br_insert_onehot.asUInt.orR, oe.brOffset.last, new_br_offset)
    old_entry_modified.pftAddr := getLower(io.start_addr) + new_pft_offset
    old_entry_modified.last_is_rvc := pd.rvcMask(new_pft_offset - 1.U) // TODO: fix this
    old_entry_modified.carry := (getLower(io.start_addr) +& new_pft_offset).head(1).asBool
    old_entry_modified.oversize := false.B
    old_entry_modified.jmpValid := false.B
  }

  val old_entry_jmp_target_modified = WireInit(io.old_entry)
  val jalr_mispredicted = cfi_is_jalr && io.mispredict_vec(io.pd.jmpOffset)
  when (jalr_mispredicted) {
    old_entry_jmp_target_modified.jmpTarget := io.target
  }

  io.new_entry := Mux(!hit, init_entry,
                    Mux(is_new_br, old_entry_modified,
                      Mux(jalr_mispredicted, old_entry_jmp_target_modified, io.old_entry)))
  io.new_br_insert_pos := new_br_insert_onehot
  val new_offset_vec = VecInit(io.new_entry.brOffset :+ pd.jmpOffset)
  val br_jal_valid_vec = VecInit(io.new_entry.brValids :+ io.new_entry.jmpValid)
  io.taken_mask := VecInit((new_offset_vec zip br_jal_valid_vec).map{
    case (off, v) => io.cfiIndex.bits === off && io.cfiIndex.valid && v
  })
  for (i <- 0 until numBr) {
    io.mispred_mask(i) := io.new_entry.brValids(i) && io.mispredict_vec(io.new_entry.brOffset(i))
  }
  io.mispred_mask.last := io.new_entry.jmpValid && io.mispredict_vec(pd.jmpOffset)

  // for perf counters
  io.is_init_entry := !hit
  io.is_old_entry := hit && !is_new_br && !jalr_mispredicted
  io.is_new_br := hit && is_new_br
  io.is_jalr_target_modified := hit && jalr_mispredicted
  io.is_br_full := hit && is_new_br && br_full
}

class Ftq(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper
  with HasBackendRedirectInfo with BPUUtils {
  val io = IO(new Bundle {
    val fromBpu = Flipped(new BpuToFtqIO)
    val fromIfu = Flipped(new IfuToFtqIO)
    val fromBackend = Flipped(new CtrlToFtqIO)
    
    val toBpu = new FtqToBpuIO
    val toIfu = new FtqToIfuIO
    val toBackend = new FtqToCtrlIO

    val bpuInfo = new Bundle {
      val bpRight = Output(UInt(XLEN.W))
      val bpWrong = Output(UInt(XLEN.W))
    }
  })
  io.bpuInfo := DontCare

  val roqFlush = io.fromBackend.roqFlush
  val stage2Redirect = io.fromBackend.stage2Redirect
  val stage3Redirect = io.fromBackend.stage3Redirect

  val stage2Flush = stage2Redirect.valid || roqFlush.valid
  val backendFlush = stage2Flush || RegNext(stage2Flush)
  val ifuFlush = Wire(Bool())

  val flush = stage2Flush || RegNext(stage2Flush)

  val allowBpuIn, allowToIfu = WireInit(false.B)
  val flushToIfu = !allowToIfu
  // all redirect except load replay
  allowBpuIn := !ifuFlush && !roqFlush.valid &&
                !(stage2Redirect.valid && !isLoadReplay(stage2Redirect)) &&
                !(stage3Redirect.valid && !isLoadReplay(stage3Redirect))
  
  allowToIfu := !ifuFlush && !roqFlush.valid && !stage2Redirect.valid && !stage3Redirect.valid
  
  val bpuPtr, ifuPtr, ifuWbPtr, commPtr = RegInit(FtqPtr(false.B, 0.U))
  val validEntries = distanceBetween(bpuPtr, commPtr)

  // **********************************************************************
  // **************************** enq from bpu ****************************
  // **********************************************************************
  io.fromBpu.resp.ready := validEntries < FtqSize.U
  val enq_fire = io.fromBpu.resp.fire() && allowBpuIn

  // read ports:                            jumpPc + redirects + loadPred + roqFlush + ifuReq + commitUpdate
  val ftq_pc_mem = Module(new SyncDataModuleTemplate(new Ftq_RF_Components, FtqSize, 1+numRedirect+2+1+1, 1))
  ftq_pc_mem.io.wen(0) := enq_fire
  ftq_pc_mem.io.waddr(0) := bpuPtr.value
  ftq_pc_mem.io.wdata(0).startAddr := io.fromBpu.resp.bits.pc
  ftq_pc_mem.io.wdata(0).nextRangeAddr := io.fromBpu.resp.bits.pc + (FetchWidth * 4).U
  ftq_pc_mem.io.wdata(0).pftAddr := io.fromBpu.resp.bits.ftb_entry.pftAddr
  ftq_pc_mem.io.wdata(0).isNextMask := VecInit((0 until PredictWidth).map(i =>
    (io.fromBpu.resp.bits.pc(log2Ceil(PredictWidth), 1) +& i.U)(log2Ceil(PredictWidth)).asBool()
  ))
  ftq_pc_mem.io.wdata(0).oversize := io.fromBpu.resp.bits.ftb_entry.oversize
  ftq_pc_mem.io.wdata(0).carry := io.fromBpu.resp.bits.ftb_entry.carry

  // read ports:                                                       ifuRedirect + backendRedirect + commit
  val ftq_hist_mem = Module(new SyncDataModuleTemplate(new GlobalHistory, FtqSize, 1+1+1, 1))
  ftq_hist_mem.io.wen(0) := enq_fire
  ftq_hist_mem.io.waddr(0) := bpuPtr.value
  ftq_hist_mem.io.wdata(0) := io.fromBpu.resp.bits.ghist
  //                                                            ifuRedirect + backendRedirect + commit
  val ftq_redirect_sram = Module(new FtqNRSRAM(new Ftq_Redirect_SRAMEntry, 1+1+1))
  ftq_redirect_sram.io.wen := enq_fire
  ftq_redirect_sram.io.waddr := bpuPtr.value
  ftq_redirect_sram.io.wdata.rasSp := io.fromBpu.resp.bits.rasSp
  ftq_redirect_sram.io.wdata.rasEntry := io.fromBpu.resp.bits.rasTop
  ftq_redirect_sram.io.wdata.specCnt := io.fromBpu.resp.bits.specCnt

  val ftq_meta_1r_sram = Module(new FtqNRSRAM(new Ftq_1R_SRAMEntry, 1))
  ftq_meta_1r_sram.io.wen := enq_fire
  ftq_meta_1r_sram.io.waddr := bpuPtr.value
  ftq_meta_1r_sram.io.wdata.meta := io.fromBpu.resp.bits.meta
  //                                                            ifuRedirect + backendRedirect + commit
  val ftb_entry_mem = Module(new SyncDataModuleTemplate(new FTBEntry, FtqSize, 1+1+1, 1))
  ftb_entry_mem.io.wen(0) := enq_fire
  ftb_entry_mem.io.waddr(0) := bpuPtr.value
  ftb_entry_mem.io.wdata(0) := io.fromBpu.resp.bits.ftb_entry

  
  // multi-write
  val update_target = Reg(Vec(FtqSize, UInt(VAddrBits.W)))
  val cfiIndex_vec = Reg(Vec(FtqSize, ValidUndirectioned(UInt(log2Ceil(PredictWidth).W))))
  val mispredict_vec = Reg(Vec(FtqSize, Vec(PredictWidth, Bool())))
  
  val c_invalid :: c_valid :: c_commited :: Nil = Enum(3)
  val commitStateQueue = RegInit(VecInit(Seq.fill(FtqSize) {
    VecInit(Seq.fill(PredictWidth)(c_invalid))
  }))
  
  val f_to_send :: f_sent :: Nil = Enum(2)
  val entry_fetch_status = RegInit(VecInit(Seq.fill(FtqSize)(f_sent)))

  val l_invalid :: l_replaying :: Nil = Enum(2)
  val entry_replay_status = RegInit(VecInit(Seq.fill(FtqSize)(l_invalid)))

  val h_not_hit :: h_false_hit :: h_hit :: Nil = Enum(3)
  val entry_hit_status = RegInit(VecInit(Seq.fill(FtqSize)(h_not_hit)))

  // 'from' is older in fetch sequence
  def set_status_between[T <: Data](status_vec: Vec[T])(from: FtqPtr, to: FtqPtr, status: T) = {
    require(status_vec.length == FtqSize)
    XSError(isAfter(from, to), "in set_status_between, \'from\' must be not after \'to\'\n")
    for (i <- 0 until FtqSize) {
      val wen = Mux(from.value < to.value,
                      i.U >= from.value && i.U < to.value,
                      i.U >= from.value || i.U < to.value) // when from.value === to.value, all entry is set
      when (wen) {
        status_vec(i) := status
      }
    }
  }
  val set_fetch_status_between = set_status_between(entry_fetch_status)(_, _, _)
  val set_commit_status_between = set_status_between(commitStateQueue)(_, _, _)
  val set_replay_status_between = set_status_between(entry_replay_status)(_, _, _)

  when (enq_fire) {
    val enqIdx = bpuPtr.value
    val preds = io.fromBpu.resp.bits.preds
    val ftb_entry = io.fromBpu.resp.bits.ftb_entry
    val real_taken_mask = preds.taken_mask.asUInt
    val enq_cfiIndex = WireInit(0.U.asTypeOf(new ValidUndirectioned(UInt(log2Ceil(PredictWidth).W))))
    entry_fetch_status(enqIdx) := f_to_send
    commitStateQueue(enqIdx) := VecInit(Seq.fill(PredictWidth)(c_invalid))
    entry_replay_status(enqIdx) := l_invalid // may be useless
    entry_hit_status(enqIdx) := Mux(io.fromBpu.resp.bits.hit, h_hit, h_not_hit) // pd may change it to h_false_hit
    enq_cfiIndex.valid := preds.taken_mask.asUInt.orR
    // when no takens, set cfiIndex to PredictWidth-1
    enq_cfiIndex.bits := ParallelPriorityMux(preds.taken_mask, ftb_entry.getOffsetVec) |
                         Fill(log2Ceil(PredictWidth), (!preds.taken_mask.asUInt.orR).asUInt)
    cfiIndex_vec(enqIdx) := enq_cfiIndex
    mispredict_vec(enqIdx) := WireInit(VecInit(Seq.fill(PredictWidth)(false.B)))
    update_target(enqIdx) := preds.target
  }
  
  bpuPtr := bpuPtr + enq_fire
  
  // *********************************************************************
  // **************************** wb from ifu ****************************
  // *********************************************************************
  val pdWb = io.fromIfu.pdWb
  val pds = pdWb.bits.pd
  val ifu_wb_valid = pdWb.valid
  val ifu_wb_idx = pdWb.bits.ftqIdx.value
  // read ports:                                                         commit update
  val ftq_pd_mem = Module(new SyncDataModuleTemplate(new Ftq_pd_Entry, FtqSize, 1, 1))
  ftq_pd_mem.io.wen(0) := ifu_wb_valid && entry_replay_status(ifu_wb_idx) =/= l_replaying
  ftq_pd_mem.io.waddr(0) := pdWb.bits.ftqIdx.value
  ftq_pd_mem.io.wdata(0).fromPdWb(pdWb.bits)

  val hit_pd_valid = entry_hit_status(ifu_wb_idx) === h_hit &&
                     entry_replay_status(ifu_wb_idx) =/= l_replaying &&
                     ifu_wb_valid
  val hit_pd_mispred = hit_pd_valid && pdWb.bits.misOffset.valid
  val hit_pd_mispred_reg = RegNext(hit_pd_mispred, init=false.B)
  val pd_reg       = RegEnable(pds,             enable = pdWb.valid)
  val start_pc_reg = RegEnable(pdWb.bits.pc(0), enable = pdWb.valid)
  val wb_idx_reg   = RegEnable(ifu_wb_idx,      enable = pdWb.valid)

  when (ifu_wb_valid) {
    val comm_stq_wen = VecInit(pds.map(_.valid).zip(pdWb.bits.instrRange).map{
      case (v, inRange) => v && inRange
    })
    (commitStateQueue(ifu_wb_idx) zip comm_stq_wen).map{
      case (qe, v) => when (v) { qe := c_valid }
    }
    entry_replay_status(ifu_wb_idx) := l_invalid
  }

  ifuWbPtr := ifuWbPtr + ifu_wb_valid

  ftb_entry_mem.io.raddr.head := ifu_wb_idx
  val has_false_hit = WireInit(false.B)
  when (RegNext(hit_pd_valid)) {
    // check for false hit
    val pred_ftb_entry = ftb_entry_mem.io.rdata.head
    // we check cfis that bpu predicted
    val br_false_hit = (pred_ftb_entry.brValids zip pred_ftb_entry.brOffset).map{
      case (v, offset) => v && !(pd_reg(offset).valid && pd_reg(offset).isBr)
    }.reduce(_||_)
    
    val jmpOffset = pred_ftb_entry.jmpOffset
    val jmp_pd = pd_reg(jmpOffset)
    val jal_false_hit = pred_ftb_entry.jmpValid &&
      ((pred_ftb_entry.isJal  && !(jmp_pd.valid && jmp_pd.isJal)) ||
       (pred_ftb_entry.isJalr && !(jmp_pd.valid && jmp_pd.isJalr)) ||
       (pred_ftb_entry.isCall && !(jmp_pd.valid && jmp_pd.isCall)) ||
       (pred_ftb_entry.isRet  && !(jmp_pd.valid && jmp_pd.isRet))
      )

    has_false_hit := br_false_hit || jal_false_hit || hit_pd_mispred_reg
  }

  when (has_false_hit) {
    entry_hit_status(wb_idx_reg) := h_false_hit
  }

  XSError(ifu_wb_valid && pdWb.bits.misOffset.valid && entry_replay_status(ifu_wb_idx) === l_replaying,
    p"unexpected predecode mispredict detected at idx: ${ifu_wb_idx} startAddr: ${Hexadecimal(pdWb.bits.pc(0))} " +
    p"misOffset: ${pdWb.bits.misOffset.bits}\n")


  // ****************************************************************
  // **************************** to ifu ****************************
  // ****************************************************************
  val ifu_req_buf = RegInit(0.U.asTypeOf(ValidUndirectioned(new FetchRequestBundle)))
  val to_buf_valid = entry_fetch_status(ifuPtr.value) === f_to_send && ifuPtr =/= bpuPtr
  // ftqIdx and ftqOffset enq
  val to_buf_fire = allowToIfu && to_buf_valid && (!ifu_req_buf.valid || io.toIfu.req.ready)
  when (to_buf_fire) {
    entry_fetch_status(ifuPtr.value) := f_sent
  }
  ifuPtr := ifuPtr + to_buf_fire
  
  when (flushToIfu) {
    ifu_req_buf.valid := false.B
  }.elsewhen (to_buf_fire) {
    ifu_req_buf.valid := true.B
  }.elsewhen (io.toIfu.req.fire()) {
    ifu_req_buf.valid := false.B
  }
  
  // read pc and target
  ftq_pc_mem.io.raddr.init.last := ifuPtr.value

  val loadReplayOffset = RegInit(0.U.asTypeOf(Valid(UInt(log2Ceil(PredictWidth).W))))
  when (to_buf_fire) {
    ifu_req_buf.bits.ftqIdx := ifuPtr
    ifu_req_buf.bits.ldReplayOffset := loadReplayOffset
    ifu_req_buf.bits.target := update_target(ifuPtr.value)
    ifu_req_buf.bits.ftqOffset := cfiIndex_vec(ifuPtr.value)
    when (loadReplayOffset.valid) {
      loadReplayOffset.valid := false.B
    }
  }

  when (RegNext(to_buf_fire)) {
    ifu_req_buf.bits.startAddr    := ftq_pc_mem.io.rdata.init.last.startAddr
    ifu_req_buf.bits.fallThruAddr := ftq_pc_mem.io.rdata.init.last.getFallThrough()
    ifu_req_buf.bits.oversize     := ftq_pc_mem.io.rdata.init.last.oversize
  }
  
  val last_cycle_to_buf_fire = RegNext(to_buf_fire)
  io.toIfu.req.valid := ifu_req_buf.valid
  io.toIfu.req.bits  := ifu_req_buf.bits
  when (last_cycle_to_buf_fire) {
    io.toIfu.req.bits.startAddr    := ftq_pc_mem.io.rdata.init.last.startAddr
    io.toIfu.req.bits.fallThruAddr := ftq_pc_mem.io.rdata.init.last.getFallThrough()
    io.toIfu.req.bits.oversize     := ftq_pc_mem.io.rdata.init.last.oversize
  }
        

  // **********************************************************************
  // **************************** backend read ****************************
  // **********************************************************************

  // pc reads
  for ((req, i) <- io.toBackend.pc_reads.zipWithIndex) {
    ftq_pc_mem.io.raddr(i) := req.ptr.value
    req.data := ftq_pc_mem.io.rdata(i).getPc(RegNext(req.offset))
  }
  // target read
  io.toBackend.target_read.data := RegNext(update_target(io.toBackend.target_read.ptr.value))
  // redirect read cfiInfo, couples to redirectGen s2
  ftq_redirect_sram.io.ren.init.last := io.fromBackend.stage2Redirect.valid
  ftq_redirect_sram.io.raddr.init.last := io.fromBackend.stage2Redirect.bits.ftqIdx.value

  ftq_hist_mem.io.raddr.init.last := io.fromBackend.stage2Redirect.bits.ftqIdx.value
  ftb_entry_mem.io.raddr.init.last := io.fromBackend.stage2Redirect.bits.ftqIdx.value

  val stage3CfiInfo = ftq_redirect_sram.io.rdata.init.last
  val fromBackendRedirect = WireInit(io.fromBackend.stage3Redirect)
  val backendRedirectCfi = fromBackendRedirect.bits.cfiUpdate
  backendRedirectCfi.hist := ftq_hist_mem.io.rdata.init.last
  backendRedirectCfi.br_hit := ftb_entry_mem.io.rdata.init.last.hasBr(fromBackendRedirect.bits.ftqOffset) &&
                               entry_hit_status(fromBackendRedirect.bits.ftqIdx.value) === h_hit
  backendRedirectCfi.rasSp := stage3CfiInfo.rasSp
  backendRedirectCfi.rasEntry := stage3CfiInfo.rasEntry
  backendRedirectCfi.specCnt := stage3CfiInfo.specCnt

  // ***************************************************************************
  // **************************** redirect from ifu ****************************
  // ***************************************************************************
  val fromIfuRedirect = WireInit(0.U.asTypeOf(Valid(new Redirect)))
  fromIfuRedirect.valid := pdWb.valid && pdWb.bits.misOffset.valid && !backendFlush
  fromIfuRedirect.bits.ftqIdx := pdWb.bits.ftqIdx
  fromIfuRedirect.bits.ftqOffset := pdWb.bits.misOffset.bits
  fromIfuRedirect.bits.level := RedirectLevel.flushAfter // TODO: is this right?

  val ifuRedirectCfiUpdate = fromIfuRedirect.bits.cfiUpdate
  ifuRedirectCfiUpdate.pc := pdWb.bits.pc(pdWb.bits.misOffset.bits)
  ifuRedirectCfiUpdate.pd := pdWb.bits.pd(pdWb.bits.misOffset.bits)
  ifuRedirectCfiUpdate.predTaken := cfiIndex_vec(pdWb.bits.ftqIdx.value).valid
  ifuRedirectCfiUpdate.target := Mux(pdWb.bits.cfiOffset.valid, pdWb.bits.target, pdWb.bits.pc(0)+(FetchWidth*4).U)
  ifuRedirectCfiUpdate.taken := pdWb.bits.cfiOffset.valid
  ifuRedirectCfiUpdate.isMisPred := pdWb.bits.misOffset.valid

  val ifuRedirectReg = RegNext(fromIfuRedirect, init=0.U.asTypeOf(Valid(new Redirect)))
  val ifuRedirectToBpu = WireInit(ifuRedirectReg)
  ifuFlush := fromIfuRedirect.valid || ifuRedirectToBpu.valid

  ftq_redirect_sram.io.ren.head := fromIfuRedirect.valid
  ftq_redirect_sram.io.raddr.head := fromIfuRedirect.bits.ftqIdx.value
  
  ftq_hist_mem.io.raddr.head := fromIfuRedirect.bits.ftqIdx.value
  ftb_entry_mem.io.raddr.head := fromIfuRedirect.bits.ftqIdx.value

  val toBpuCfi = ifuRedirectToBpu.bits.cfiUpdate
  toBpuCfi.hist := ftq_hist_mem.io.rdata.head
  toBpuCfi.br_hit := ftb_entry_mem.io.rdata.head.hasBr(ifuRedirectToBpu.bits.ftqOffset) &&
                     entry_hit_status(ifuRedirectToBpu.bits.ftqIdx.value) === h_hit &&
                     !has_false_hit
  toBpuCfi.rasSp := ftq_redirect_sram.io.rdata.head.rasSp
  toBpuCfi.rasEntry := ftq_redirect_sram.io.rdata.head.rasEntry
  toBpuCfi.specCnt := ftq_redirect_sram.io.rdata.head.specCnt
  when (ifuRedirectReg.bits.cfiUpdate.pd.isRet) {
    toBpuCfi.target := toBpuCfi.rasEntry.retAddr
  }
  // TODO: sawNotTakenBranch

  // *********************************************************************                                  
  // **************************** wb from exu ****************************
  // *********************************************************************

  def extractRedirectInfo(wb: Valid[Redirect]) = {
    val ftqIdx = wb.bits.ftqIdx.value
    val ftqOffset = wb.bits.ftqOffset
    val taken = wb.bits.cfiUpdate.taken
    val mispred = wb.bits.cfiUpdate.isMisPred
    (wb.valid, ftqIdx, ftqOffset, taken, mispred)
  }

  // fix mispredict entry
  val lastIsMispredict = RegNext(
    stage2Redirect.valid && stage2Redirect.bits.level === RedirectLevel.flushAfter, init = false.B
  )

  def updateCfiInfo(redirect: Valid[Redirect], isBackend: Boolean = true) = {
    val r = extractRedirectInfo(redirect)
    val idx = r._2
    val cfiIndex_bits_wen = r._1 && r._4 && r._3 < cfiIndex_vec(r._2).bits
    val cfiIndex_valid_wen = r._1 && r._3 === cfiIndex_vec(r._2).bits
    when (cfiIndex_bits_wen || cfiIndex_valid_wen) {
      cfiIndex_vec(idx).valid := cfiIndex_bits_wen || cfiIndex_valid_wen && r._4
    }
    when (cfiIndex_bits_wen) {
      cfiIndex_vec(idx).bits := r._3
    }
    update_target(idx) := redirect.bits.cfiUpdate.target
    if (isBackend) {
      mispredict_vec(r._2)(r._3) := r._5
    }
  }

  when(stage3Redirect.valid && lastIsMispredict) {
    updateCfiInfo(stage3Redirect)
  }.elsewhen (ifuRedirectToBpu.valid) {
    updateCfiInfo(ifuRedirectToBpu, isBackend=false)
  }

  // ***********************************************************************************
  // **************************** flush ptr and state queue ****************************
  // ***********************************************************************************

  class RedirectInfo extends Bundle {
    val valid = Bool()
    val ftqIdx = new FtqPtr
    val ftqOffset = UInt(4.W)
    val flushItSelf = Bool()
    def apply(valid: Bool, idx: FtqPtr, offset: UInt, flushItSelf: Bool = false.B) = {
      this.valid := valid
      this.ftqIdx := idx
      this.ftqOffset := offset
      this.flushItSelf := flushItSelf
      this
    }
  }
  val redirectVec = Wire(Vec(3, new RedirectInfo))
  redirectVec(0).apply(
    roqFlush.valid,
    roqFlush.bits.ftqIdx,
    roqFlush.bits.ftqOffset
  )
  redirectVec(1).apply(
    stage2Redirect.valid,
    stage2Redirect.bits.ftqIdx,
    stage2Redirect.bits.ftqOffset,
    RedirectLevel.flushItself(stage2Redirect.bits.level)
  )
  redirectVec(2).apply(
    fromIfuRedirect.valid,
    fromIfuRedirect.bits.ftqIdx,
    fromIfuRedirect.bits.ftqOffset
  )
  
  // when redirect, we should reset ptrs and status queues

  // For normal redirects, the inst casusing redirect itself
  // should not be flushed (called flushAfter), we should reset
  // both bpuPtr and ifuPtr, and send redirect request to both module.

  // However, for load replays, the load instruction itself should be
  // re-fetched and re-executed. In this case we only send another
  // fetch request to ifu, fetching instructions starting from this load,
  // resetting ifuPtr. We do not trigger a redirect for bpu, and we reuse
  // previously predicted results to provide the following fetch stream


  when(redirectVec.map(r => r.valid).reduce(_||_)){
    val r = PriorityMux(redirectVec.map(r => (r.valid -> r)))
    val notIfu = redirectVec.dropRight(1).map(r => r.valid).reduce(_||_)
    val (idx, offset, flushItSelf) = (r.ftqIdx, r.ftqOffset, r.flushItSelf)
    val next = idx + 1.U
    when (!flushItSelf) {
      bpuPtr := next
      ifuPtr := next
      ifuWbPtr := next
      when (notIfu) {
        commitStateQueue(idx.value).zipWithIndex.foreach({ case (s, i) =>
          when(i.U > offset){
            s := c_invalid
          }
        })
        when(next.value =/= commPtr.value){ // if next.value === commPtr.value, ftq is full
          commitStateQueue(next.value).foreach(_ := c_invalid)
        }
      }
      set_replay_status_between(ifuPtr, ifuPtr, l_invalid) // set all to invalid
      loadReplayOffset.valid := false.B
    // load replay
    }.otherwise {
      ifuPtr := idx
      ifuWbPtr := idx
      // set fetch status of entries between ifuPtr and bpuPtr to f_to_send
      set_fetch_status_between(idx, ifuPtr, f_to_send)
      // set commit state of entries between ifuWbPtr and bpuPtr to c_invalid
      set_commit_status_between(idx+1.U, ifuWbPtr, VecInit(Seq.fill(PredictWidth)(c_invalid)))
      // set replay status
      set_replay_status_between(idx, ifuWbPtr, l_replaying)
      // set load replay offset
      loadReplayOffset.valid := true.B
      loadReplayOffset.bits := offset
    }
  }

  // only the valid bit is actually needed
  io.toIfu.redirect := DontCare
  io.toIfu.redirect.valid := stage2Flush
  
  // commit
  for (c <- io.fromBackend.roq_commits) {
    when(c.valid) {
      commitStateQueue(c.bits.ftqIdx.value)(c.bits.ftqOffset) := c_commited
    }
  }
  
  // ****************************************************************
  // **************************** to bpu ****************************
  // ****************************************************************
  
  // do not send redirect to bpu when load replay
  io.toBpu.redirect <> 
    Mux(fromBackendRedirect.valid && !fromBackendRedirect.bits.flushItself,
      fromBackendRedirect,
      ifuRedirectToBpu)
  
  val do_commit = Wire(Bool())
  val canCommit = commPtr =/= ifuWbPtr && !do_commit &&
    Cat(commitStateQueue(commPtr.value).map(s => {
      s === c_invalid || s === c_commited
    })).andR()

  // need one cycle to read mem and srams 
  do_commit := RegNext(canCommit, init=false.B)
  when (do_commit) { commPtr := commPtr + 1.U }
  val commit_hit = entry_hit_status(commPtr.value)
  val commit_entry_taken = cfiIndex_vec(commPtr.value).valid
  val commit_valid = commit_hit === h_hit || commit_entry_taken // hit or taken
  

  ftq_pc_mem.io.raddr.last := commPtr.value
  val commit_pc_bundle = ftq_pc_mem.io.rdata.last
  ftq_hist_mem.io.raddr.last := commPtr.value
  val commit_ghist = ftq_hist_mem.io.rdata.last
  ftq_pd_mem.io.raddr.last := commPtr.value
  val commit_pd = ftq_pd_mem.io.rdata.last
  ftq_redirect_sram.io.ren.last := canCommit
  ftq_redirect_sram.io.raddr.last := commPtr.value
  val commit_spec_meta = ftq_redirect_sram.io.rdata.last
  ftq_meta_1r_sram.io.ren(0) := canCommit
  ftq_meta_1r_sram.io.raddr(0) := commPtr.value
  val commit_meta = ftq_meta_1r_sram.io.rdata(0)
  ftb_entry_mem.io.raddr.last := commPtr.value
  val commit_ftb_entry = ftb_entry_mem.io.rdata.last

  io.toBpu.update := DontCare
  io.toBpu.update.valid := commit_valid && do_commit
  val update = io.toBpu.update.bits
  update.false_hit := commit_hit === h_false_hit
  update.pc := commit_pc_bundle.startAddr
  update.hit := commit_hit === h_hit || commit_hit === h_false_hit
  update.ghist := commit_ghist
  update.rasSp := commit_spec_meta.rasSp
  update.rasTop := commit_spec_meta.rasEntry
  update.specCnt := commit_spec_meta.specCnt
  update.meta := commit_meta.meta
  
  val commit_real_hit = commit_hit === h_hit
  val update_ftb_entry = update.ftb_entry
  
  val ftbEntryGen = Module(new FTBEntryGen).io
  ftbEntryGen.start_addr := commit_pc_bundle.startAddr
  ftbEntryGen.old_entry := commit_ftb_entry
  ftbEntryGen.pd := commit_pd
  ftbEntryGen.cfiIndex := cfiIndex_vec(commPtr.value)
  ftbEntryGen.target := update_target(commPtr.value)
  ftbEntryGen.hit := commit_real_hit
  ftbEntryGen.mispredict_vec := mispredict_vec(commPtr.value)
  
  update_ftb_entry := ftbEntryGen.new_entry
  update.new_br_insert_pos := ftbEntryGen.new_br_insert_pos
  update.mispred_mask := ftbEntryGen.mispred_mask

  val preds = update.preds
  preds.is_br := update_ftb_entry.brValids
  preds.is_jal := update_ftb_entry.jmpValid && !update_ftb_entry.isJalr
  preds.is_jalr := update_ftb_entry.jmpValid && update_ftb_entry.isJalr
  preds.is_call := update_ftb_entry.jmpValid && update_ftb_entry.isCall
  preds.is_ret  := update_ftb_entry.jmpValid && update_ftb_entry.isRet
  preds.call_is_rvc := update_ftb_entry.jmpValid && update_ftb_entry.isCall && update_ftb_entry.last_is_rvc
  preds.target := update_target(commPtr.value)
  preds.taken_mask := ftbEntryGen.taken_mask


  val enq = io.fromBpu.resp
  val perf_redirect = io.fromBackend.stage2Redirect
  
  if (!env.FPGAPlatform && env.EnablePerfDebug) {
    XSPerfAccumulate("entry", validEntries)
    XSPerfAccumulate("stall", enq.valid && !enq.ready)
    XSPerfAccumulate("mispredictRedirect", perf_redirect.valid && RedirectLevel.flushAfter === perf_redirect.bits.level)
    XSPerfAccumulate("replayRedirect", perf_redirect.valid && RedirectLevel.flushItself(perf_redirect.bits.level))

    XSPerfAccumulate("toIfuBubble", io.toIfu.req.ready && !io.toIfu.req.valid)

    val from_bpu = io.fromBpu.resp.bits
    val enq_entry_len = (from_bpu.ftb_entry.getFallThrough(from_bpu.pc) - from_bpu.pc) >> instOffsetBits
    val enq_entry_len_recording_vec = (1 to PredictWidth+1).map(i => enq_entry_len === i.U)
    val enq_entry_len_map = (1 to PredictWidth+1).map(i =>
      f"enq_ftb_entry_len_$i" -> (enq_entry_len_recording_vec(i-1) && enq_fire)
    ).foldLeft(Map[String, UInt]())(_+_)

    val to_ifu = io.toIfu.req.bits
    val to_ifu_entry_len = (to_ifu.fallThruAddr - to_ifu.startAddr) >> instOffsetBits
    val to_ifu_entry_len_recording_vec = (1 to PredictWidth+1).map(i => to_ifu_entry_len === i.U)
    val to_ifu_entry_len_map = (1 to PredictWidth+1).map(i =>
      f"to_ifu_ftb_entry_len_$i" -> (to_ifu_entry_len_recording_vec(i-1) && io.toIfu.req.fire)
    ).foldLeft(Map[String, UInt]())(_+_)

    // commit perf counters
    val commit_inst_mask    = VecInit(commitStateQueue(commPtr.value).map(c => c === c_commited && do_commit)).asUInt
    val commit_mispred_mask = mispredict_vec(commPtr.value).asUInt
    val commit_not_mispred_mask = ~commit_mispred_mask

    val commit_br_mask = commit_pd.brMask.asUInt
    val commit_jmp_mask = UIntToOH(commit_pd.jmpOffset) & Fill(PredictWidth, commit_pd.jmpInfo.valid.asTypeOf(UInt(1.W)))
    val commit_cfi_mask = (commit_br_mask | commit_jmp_mask)

    val commit_jal_mask  = UIntToOH(commit_pd.jmpOffset) & Fill(PredictWidth, commit_pd.hasJal.asTypeOf(UInt(1.W)))
    val commit_jalr_mask = UIntToOH(commit_pd.jmpOffset) & Fill(PredictWidth, commit_pd.hasJalr.asTypeOf(UInt(1.W)))
    val commit_call_mask = UIntToOH(commit_pd.jmpOffset) & Fill(PredictWidth, commit_pd.hasCall.asTypeOf(UInt(1.W)))
    val commit_ret_mask  = UIntToOH(commit_pd.jmpOffset) & Fill(PredictWidth, commit_pd.hasRet.asTypeOf(UInt(1.W)))
    
    val mbpInstrs = commit_inst_mask & commit_cfi_mask

    val mbpRights = commit_inst_mask & commit_not_mispred_mask
    val mbpBRights = mbpRights & commit_br_mask
    val mbpJRights = mbpRights & commit_jal_mask
    val mbpIRights = mbpRights & commit_jalr_mask
    val mbpCRights = mbpRights & commit_call_mask
    val mbpRRights = mbpRights & commit_ret_mask

    val mbpWrongs = commit_inst_mask & commit_mispred_mask
    val mbpBWrongs = mbpWrongs & commit_br_mask
    val mbpJWrongs = mbpWrongs & commit_jal_mask
    val mbpIWrongs = mbpWrongs & commit_jalr_mask
    val mbpCWrongs = mbpWrongs & commit_call_mask
    val mbpRWrongs = mbpWrongs & commit_ret_mask

    val update_valid = io.toBpu.update.valid
    def u(cond: Bool) = update_valid && cond
    val ftb_false_hit = u(update.false_hit)
    val ftb_hit = u(commit_hit === h_hit)

    val ftb_new_entry = u(ftbEntryGen.is_init_entry)
    val ftb_new_entry_only_br = ftb_new_entry && !update.ftb_entry.jmpValid
    val ftb_new_entry_only_jmp = ftb_new_entry && !update.ftb_entry.brValids(0)
    val ftb_new_entry_has_br_and_jmp = ftb_new_entry && update.ftb_entry.brValids(0) && update.ftb_entry.jmpValid

    val ftb_old_entry = u(ftbEntryGen.is_old_entry)
    
    val ftb_modified_entry = u(ftbEntryGen.is_new_br || ftbEntryGen.is_jalr_target_modified)
    val ftb_modified_entry_new_br = u(ftbEntryGen.is_new_br)
    val ftb_modified_entry_jalr_target_modified = u(ftbEntryGen.is_jalr_target_modified)
    val ftb_modified_entry_br_full = ftb_modified_entry && ftbEntryGen.is_br_full

    val ftb_entry_len = (ftbEntryGen.new_entry.getFallThrough(update.pc) - update.pc) >> instOffsetBits
    val ftb_entry_len_recording_vec = (1 to PredictWidth+1).map(i => ftb_entry_len === i.U)
    val ftb_init_entry_len_map = (1 to PredictWidth+1).map(i =>
      f"ftb_init_entry_len_$i" -> (ftb_entry_len_recording_vec(i-1) && ftb_new_entry)
    ).foldLeft(Map[String, UInt]())(_+_)
    val ftb_modified_entry_len_map = (1 to PredictWidth+1).map(i =>
      f"ftb_modified_entry_len_$i" -> (ftb_entry_len_recording_vec(i-1) && ftb_modified_entry)
    ).foldLeft(Map[String, UInt]())(_+_)

    val perfCountsMap = Map(
      "BpInstr" -> PopCount(mbpInstrs),
      "BpBInstr" -> PopCount(mbpBRights | mbpBWrongs),
      "BpRight"  -> PopCount(mbpRights),
      "BpWrong"  -> PopCount(mbpWrongs),
      "BpBRight" -> PopCount(mbpBRights),
      "BpBWrong" -> PopCount(mbpBWrongs),
      "BpJRight" -> PopCount(mbpJRights),
      "BpJWrong" -> PopCount(mbpJWrongs),
      "BpIRight" -> PopCount(mbpIRights),
      "BpIWrong" -> PopCount(mbpIWrongs),
      "BpCRight" -> PopCount(mbpCRights),
      "BpCWrong" -> PopCount(mbpCWrongs),
      "BpRRight" -> PopCount(mbpRRights),
      "BpRWrong" -> PopCount(mbpRWrongs),

      // "ubtbRight" -> PopCount(ubtbRights),
      // "ubtbWrong" -> PopCount(ubtbWrongs),
      // "btbRight" -> PopCount(btbRights),
      // "btbWrong" -> PopCount(btbWrongs),
      // "tageRight" -> PopCount(tageRights),
      // "tageWrong" -> PopCount(tageWrongs),

      // "rasRight"  -> PopCount(rasRights),
      // "rasWrong"  -> PopCount(rasWrongs),
      // "loopRight" -> PopCount(loopRights),
      // "loopWrong" -> PopCount(loopWrongs),
      "ftb_false_hit"                -> PopCount(ftb_false_hit),
      "ftb_hit"                      -> PopCount(ftb_hit),
      "ftb_new_entry"                -> PopCount(ftb_new_entry),
      "ftb_new_entry_only_br"        -> PopCount(ftb_new_entry_only_br),
      "ftb_new_entry_only_jmp"       -> PopCount(ftb_new_entry_only_jmp),
      "ftb_new_entry_has_br_and_jmp" -> PopCount(ftb_new_entry_has_br_and_jmp),
      "ftb_old_entry"                -> PopCount(ftb_old_entry),
      "ftb_modified_entry"           -> PopCount(ftb_modified_entry),
      "ftb_modified_entry_new_br"    -> PopCount(ftb_modified_entry_new_br),
      "ftb_jalr_target_modified"     -> PopCount(ftb_modified_entry_jalr_target_modified),
      "ftb_modified_entry_br_full"   -> PopCount(ftb_modified_entry_br_full)
    ) ++ ftb_init_entry_len_map ++ ftb_modified_entry_len_map ++ enq_entry_len_map ++
    to_ifu_entry_len_map

    for((key, value) <- perfCountsMap) {
      XSPerfAccumulate(key, value)
    }

    // XSError(mbpJWrongs.orR, p"commit detetced jal misprediction! " +
    //   p"commPtr: $commPtr, startAddr: ${commit_pc_bundle.startAddr}, offset: ${PriorityEncoder(mbpJWrongs)}\n")

  }
  // val predRights = (0 until PredictWidth).map{i => !commitEntry.mispred(i) && !commitEntry.pd(i).notCFI && commitEntry.valids(i)}
  // val predWrongs = (0 until PredictWidth).map{i => commitEntry.mispred(i) && !commitEntry.pd(i).notCFI && commitEntry.valids(i)}

  // // Branch Predictor Perf counters
  // if (!env.FPGAPlatform && env.EnablePerfDebug) {
  //   val fires = commitEntry.valids.zip(commitEntry.pd).map{case (valid, pd) => valid && !pd.notCFI}
  //   val isBTypes = (0 until PredictWidth).map{i => commitEntry.pd(i).isBr}
  //   val isJTypes = (0 until PredictWidth).map{i => commitEntry.pd(i).isJal}
  //   val isITypes = (0 until PredictWidth).map{i => commitEntry.pd(i).isJalr}
  //   val isCTypes = (0 until PredictWidth).map{i => commitEntry.pd(i).isCall}
  //   val isRTypes = (0 until PredictWidth).map{i => commitEntry.pd(i).isRet}

  //   val mbpInstrs = fires
  //   val mbpRights = predRights
  //   val mbpWrongs = predWrongs
  //   val mbpBRights = Cat(predRights) & Cat(isBTypes)
  //   val mbpBWrongs = Cat(predWrongs) & Cat(isBTypes)
  //   val mbpJRights = Cat(predRights) & Cat(isJTypes)
  //   val mbpJWrongs = Cat(predWrongs) & Cat(isJTypes)
  //   val mbpIRights = Cat(predRights) & Cat(isITypes)
  //   val mbpIWrongs = Cat(predWrongs) & Cat(isITypes)
  //   val mbpCRights = Cat(predRights) & Cat(isCTypes)
  //   val mbpCWrongs = Cat(predWrongs) & Cat(isCTypes)
  //   val mbpRRights = Cat(predRights) & Cat(isRTypes)
  //   val mbpRWrongs = Cat(predWrongs) & Cat(isRTypes)

  //   def ubtbCheck(commit: FtqEntry, predAns: Seq[PredictorAnswer], isWrong: Bool) = {
  //     commit.valids.zip(commit.pd).zip(predAns).zip(commit.takens).map {
  //       case (((valid, pd), ans), taken) =>
  //       Mux(valid && pd.isBr,
  //         isWrong ^ Mux(ans.hit.asBool,
  //           Mux(ans.taken.asBool, taken && ans.target === commitEntry.target,
  //           !taken),
  //         !taken),
  //       false.B)
  //     }
  //   }

  //   def btbCheck(commit: FtqEntry, predAns: Seq[PredictorAnswer], isWrong: Bool) = {
  //     commit.valids.zip(commit.pd).zip(predAns).zip(commit.takens).map {
  //       case (((valid, pd), ans), taken) =>
  //       Mux(valid && pd.isBr,
  //         isWrong ^ Mux(ans.hit.asBool,
  //           Mux(ans.taken.asBool, taken && ans.target === commitEntry.target,
  //           !taken),
  //         !taken),
  //       false.B)
  //     }
  //   }

  //   def tageCheck(commit: FtqEntry, predAns: Seq[PredictorAnswer], isWrong: Bool) = {
  //     commit.valids.zip(commit.pd).zip(predAns).zip(commit.takens).map {
  //       case (((valid, pd), ans), taken) =>
  //       Mux(valid && pd.isBr,
  //         isWrong ^ (ans.taken.asBool === taken),
  //       false.B)
  //     }
  //   }

  //   def loopCheck(commit: FtqEntry, predAns: Seq[PredictorAnswer], isWrong: Bool) = {
  //     commit.valids.zip(commit.pd).zip(predAns).zip(commit.takens).map {
  //       case (((valid, pd), ans), taken) =>
  //       Mux(valid && (pd.isBr) && ans.hit.asBool, 
  //         isWrong ^ (!taken),
  //           false.B)
  //     }
  //   }

  //   def rasCheck(commit: FtqEntry, predAns: Seq[PredictorAnswer], isWrong: Bool) = {
  //     commit.valids.zip(commit.pd).zip(predAns).zip(commit.takens).map {
  //       case (((valid, pd), ans), taken) =>
  //       Mux(valid && pd.isRet.asBool /*&& taken*/ && ans.hit.asBool,
  //         isWrong ^ (ans.target === commitEntry.target),
  //           false.B)
  //     }
  //   }

  //   val ubtbRights = ubtbCheck(commitEntry, commitEntry.metas.map(_.ubtbAns), false.B)
  //   val ubtbWrongs = ubtbCheck(commitEntry, commitEntry.metas.map(_.ubtbAns), true.B)
  //   // btb and ubtb pred jal and jalr as well
  //   val btbRights = btbCheck(commitEntry, commitEntry.metas.map(_.btbAns), false.B)
  //   val btbWrongs = btbCheck(commitEntry, commitEntry.metas.map(_.btbAns), true.B)
  //   val tageRights = tageCheck(commitEntry, commitEntry.metas.map(_.tageAns), false.B)
  //   val tageWrongs = tageCheck(commitEntry, commitEntry.metas.map(_.tageAns), true.B)

  //   val loopRights = loopCheck(commitEntry, commitEntry.metas.map(_.loopAns), false.B)
  //   val loopWrongs = loopCheck(commitEntry, commitEntry.metas.map(_.loopAns), true.B)

  //   val rasRights = rasCheck(commitEntry, commitEntry.metas.map(_.rasAns), false.B)
  //   val rasWrongs = rasCheck(commitEntry, commitEntry.metas.map(_.rasAns), true.B)

  //   val perfCountsMap = Map(
  //     "BpInstr" -> PopCount(mbpInstrs),
  //     "BpBInstr" -> PopCount(commitEntry.valids.zip(commitEntry.pd).map{case (valid, pd) => valid && pd.isBr}),
  //     "BpRight"  -> PopCount(mbpRights),
  //     "BpWrong"  -> PopCount(mbpWrongs),
  //     "BpBRight" -> PopCount(mbpBRights),
  //     "BpBWrong" -> PopCount(mbpBWrongs),
  //     "BpJRight" -> PopCount(mbpJRights),
  //     "BpJWrong" -> PopCount(mbpJWrongs),
  //     "BpIRight" -> PopCount(mbpIRights),
  //     "BpIWrong" -> PopCount(mbpIWrongs),
  //     "BpCRight" -> PopCount(mbpCRights),
  //     "BpCWrong" -> PopCount(mbpCWrongs),
  //     "BpRRight" -> PopCount(mbpRRights),
  //     "BpRWrong" -> PopCount(mbpRWrongs),

  //     "ubtbRight" -> PopCount(ubtbRights),
  //     "ubtbWrong" -> PopCount(ubtbWrongs),
  //     "btbRight" -> PopCount(btbRights),
  //     "btbWrong" -> PopCount(btbWrongs),
  //     "tageRight" -> PopCount(tageRights),
  //     "tageWrong" -> PopCount(tageWrongs),

  //     "rasRight"  -> PopCount(rasRights),
  //     "rasWrong"  -> PopCount(rasWrongs),
  //     "loopRight" -> PopCount(loopRights),
  //     "loopWrong" -> PopCount(loopWrongs),
  //   )

  //   for((key, value) <- perfCountsMap) {
  //     XSPerfAccumulate(key, value)
  //   }
  // }

  // XSDebug(io.commit_ftqEntry.valid, p"ftq commit: ${io.commit_ftqEntry.bits}")
  XSDebug(enq_fire, p"ftq enq: ${enq.bits}")

  // io.bpuInfo.bpRight := PopCount(predRights)
  // io.bpuInfo.bpWrong := PopCount(predWrongs)

  // --------------------------- Debug --------------------------------
  XSDebug(enq_fire, p"enq! " + io.fromBpu.resp.bits.toPrintable)
  XSDebug(io.toIfu.req.fire, p"fire to ifu " + io.toIfu.req.bits.toPrintable)
  XSDebug(do_commit, p"deq! [ptr] $commPtr\n")
  XSDebug(true.B, p"[bpuPtr] $bpuPtr, [ifuPtr] $ifuPtr, [commPtr] $commPtr\n")
  XSDebug(true.B, p"[in] v:${io.fromBpu.resp.valid} r:${io.fromBpu.resp.ready} " +
    p"[out] v:${io.toIfu.req.valid} r:${io.toIfu.req.ready}\n")

}