package xiangshan.frontend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._

class FetchRequestBundle(implicit p: Parameters) extends XSBundle {
  val startAddr    = UInt(VAddrBits.W)
  val fallThruAddr = UInt(VAddrBits.W)
  val ftqIdx       = new FtqPtr
  val ldReplayOffset = ValidUndirectioned(UInt(log2Ceil(32).W))
  val ftqOffset    = ValidUndirectioned(UInt(log2Ceil(32).W))
  val target       = UInt(VAddrBits.W)
  val oversize     = Bool()

  override def toPrintable: Printable = {
    p"[start] ${Hexadecimal(startAddr)} [pft] ${Hexadecimal(fallThruAddr)}" +
      p"[tgt] ${Hexadecimal(target)} [ftqIdx] $ftqIdx [jmp] v:${ftqOffset.valid}" +
      p" offset: ${ftqOffset.bits}\n"
  }
}

class PredecodeWritebackBundle(implicit p:Parameters) extends XSBundle {
  val pc           = Vec(16, UInt(VAddrBits.W))
  val pd           = Vec(16, new PreDecodeInfo) // TODO: redefine Predecode
  val ftqIdx       = new FtqPtr
  val ftqOffset    = UInt(log2Ceil(16).W)
  val misOffset    = ValidUndirectioned(UInt(4.W))
  val cfiOffset    = ValidUndirectioned(UInt(4.W))
  val target       = UInt(VAddrBits.W)
  val jalTarget    = UInt(VAddrBits.W)
  val instrRange   = Vec(16, Bool())
}

class Exception(implicit p: Parameters) extends XSBundle {

}

class FetchToIBuffer(implicit p: Parameters) extends XSBundle {
  val instrs    = Vec(16, UInt(32.W))
  val valid     = UInt(16.W)
  val pd        = Vec(16, new PreDecodeInfo)
  val pc        = Vec(16, UInt(VAddrBits.W))
  val foldpc    = Vec(16, UInt(MemPredPCWidth.W))
  //val exception = new Exception
  val ftqPtr       = new FtqPtr
  val ftqOffset    = Vec(16, ValidUndirectioned(UInt(log2Ceil(16).W)))

}

// Move from BPU
class GlobalHistory(implicit p: Parameters) extends XSBundle {
  val predHist = UInt(HistoryLength.W)
  def update(sawNTBr: Bool, takenOnBr: Bool, hist: UInt = predHist): GlobalHistory = {
    val g = Wire(new GlobalHistory)
    val shifted = takenOnBr || sawNTBr
    g.predHist := Mux(shifted, (hist << 1) | takenOnBr.asUInt, hist)
    g
  }

  final def === (that: GlobalHistory): Bool = {
    predHist === that.predHist
  }

  final def =/= (that: GlobalHistory): Bool = !(this === that)

  implicit val name = "IFU"
  def debug(where: String) = XSDebug(p"[${where}_GlobalHistory] hist=${Binary(predHist)}\n")
  // override def toString(): String = "histPtr=%d, sawNTBr=%d, takenOnBr=%d, saveHalfRVI=%d".format(histPtr, sawNTBr, takenOnBr, saveHalfRVI)
}

class TableAddr(val idxBits: Int, val banks: Int)(implicit p: Parameters) extends XSBundle{
  def tagBits = VAddrBits - idxBits - instOffsetBits

  val tag = UInt(tagBits.W)
  val idx = UInt(idxBits.W)
  val offset = UInt(instOffsetBits.W)

  def fromUInt(x: UInt) = x.asTypeOf(UInt(VAddrBits.W)).asTypeOf(this)
  def getTag(x: UInt) = fromUInt(x).tag
  def getIdx(x: UInt) = fromUInt(x).idx
  def getBank(x: UInt) = if (banks > 1) getIdx(x)(log2Up(banks) - 1, 0) else 0.U
  def getBankIdx(x: UInt) = if (banks > 1) getIdx(x)(idxBits - 1, log2Up(banks)) else getIdx(x)
}
class BranchPredictionBase(implicit p: Parameters) extends XSBundle with HasBPUConst {
  val is_jal = Bool()
  val is_jalr = Bool()
  val is_call = Bool()
  val is_ret = Bool()
  val call_is_rvc = Bool()
  val target = UInt(VAddrBits.W)
  val hit = Bool()
}

class BranchPrediction(implicit p: Parameters) extends BranchPredictionBase {
  val taken_mask = Vec(numBr+1, Bool())
  val is_br = Vec(numBr, Bool())
  def taken = taken_mask.reduce(_||_) // || (is_jal || is_jalr)
  
  def hit_taken_on_call = !VecInit(taken_mask.take(numBr)).asUInt.orR && hit && is_call
  def hit_taken_on_ret  = !VecInit(taken_mask.take(numBr)).asUInt.orR && hit && is_ret
  
  override def toPrintable: Printable = {
    p"-----------BranchPrediction----------- " +
      p"[taken_mask] ${Binary(taken_mask.asUInt)} " +
      p"[is_br] ${Binary(is_br.asUInt)}, [is_jal] ${Binary(is_jal.asUInt)} " +
      p"[is_jalr] ${Binary(is_jalr.asUInt)}, [is_call] ${Binary(is_call.asUInt)}, [is_ret] ${Binary(is_ret.asUInt)} " +
      p"[target] ${Hexadecimal(target)}}, [hit] $hit "
  }
}

class BranchPredictionWrapper(implicit p: Parameters) extends BranchPredictionBase {
  val taken_mask = UInt((numBr+1).W)
  val is_br = UInt(numBr.W)
}

class BranchPredictionBundleBase(implicit p: Parameters) extends XSBundle with HasBPUConst {
  val pc = UInt(VAddrBits.W)
  val hit = Bool()

  val ghist = new GlobalHistory()
  val rasSp = UInt(log2Ceil(RasSize).W)
  val rasTop = new RASEntry
  val meta = UInt(MaxMetaLength.W)

  val ftb_entry = new FTBEntry() // TODO: Send this entry to ftq
}

class BranchPredictionBundle(implicit p: Parameters) extends BranchPredictionBundleBase {
  val preds = new BranchPrediction
  val specCnt = Vec(numBr, UInt(10.W))
  override def toPrintable: Printable = {
    p"-----------BranchPredictionBundle----------- " +
      p"[pc] ${Hexadecimal(pc)} [hit] $hit " +
      p"[ghist] ${Binary(ghist.predHist)}  " +
      preds.toPrintable +
      ftb_entry.toPrintable
  }
}

class BranchPredictionBundleWrapper(implicit p: Parameters) extends BranchPredictionBundleBase {
  val preds = new BranchPredictionWrapper
  val specCnt = UInt((numBr * 10).W)
  override def toPrintable: Printable = {
    p"-----------BranchPredictionBundle----------- " +
      p"[pc] ${Hexadecimal(pc)} [hit] $hit " +
      p"[ghist] ${Binary(ghist.predHist)}  " +
      preds.toPrintable +
      ftb_entry.toPrintable
  }
}

class BranchPredictionResp(implicit p: Parameters) extends XSBundle with HasBPUConst {
  // val valids = Vec(3, Bool())
  val s1 = new BranchPredictionBundle()
  val s2 = new BranchPredictionBundle()
  val s3 = new BranchPredictionBundle()
}

class BranchPredictionUpdateBase(implicit p: Parameters) extends BranchPredictionBundle with HasBPUConst {
  val false_hit = Bool()
  // val ghist = new GlobalHistory() This in spec_meta
}

class BranchPredictionUpdateBaseWrapper(implicit p: Parameters) extends BranchPredictionBundleWrapper with HasBPUConst {
  val false_hit = Bool()
  // val ghist = new GlobalHistory() This in spec_meta
}

class BranchPredictionUpdate(implicit p: Parameters) extends BranchPredictionUpdateBase {
  val mispred_mask = Vec(numBr+1, Bool())
  val new_br_insert_pos = Vec(numBr, Bool())
  override def toPrintable: Printable = {
    p"-----------BranchPredictionUpdate----------- " +
      p"[mispred_mask] ${Binary(mispred_mask.asUInt)} [false_hit] ${Binary(false_hit)} " +
      p"[new_br_insert_pos] ${Binary(new_br_insert_pos.asUInt)} " +
      super.toPrintable +
      p"\n"
  }
}

class BranchPredictionUpdateWrapper(implicit p: Parameters) extends BranchPredictionUpdateBaseWrapper {
  val mispred_mask = UInt((numBr+1).W)
  val new_br_insert_pos = UInt(numBr.W)
}

class BranchPredictionRedirect(implicit p: Parameters) extends Redirect with HasBPUConst {
  override def toPrintable: Printable = {
    p"-----------BranchPredictionRedirect----------- " +
      p"-----------cfiUpdate----------- " +
      p"[pc] ${Hexadecimal(cfiUpdate.pc)} " +
      p"[predTaken] ${cfiUpdate.predTaken}, [taken] ${cfiUpdate.taken}, [isMisPred] ${cfiUpdate.isMisPred} " +
      p"[target] ${cfiUpdate.target} " +
      p"------------------------------- " +
      p"[roqPtr] f=${roqIdx.flag} v=${roqIdx.value} " +
      p"[ftqPtr] f=${ftqIdx.flag} v=${ftqIdx.value} " +
      p"[ftqOffset] ${ftqOffset} " +
      p"[level] ${level}, [interrupt] ${interrupt} " +
      p"[stFtqIdx] f=${stFtqIdx.flag} v=${stFtqIdx.value} " +
      p"[stFtqOffset] ${stFtqOffset} " +
      p"\n"

  }
}

class BranchPredictionRedirectWrapper(implicit p: Parameters) extends RedirectWrapper with HasBPUConst {}