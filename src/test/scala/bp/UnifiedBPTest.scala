package bp

import bp.RespRelations.{RespMis, RespNone, RespRelation, RespRightFollow, RespRightNoFollow}
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chiseltest.experimental.TestOptionBuilder.ChiselScalatestOptionBuilder
import chiseltest.internal.{LineCoverageAnnotation, ToggleCoverageAnnotation, VerilatorBackendAnnotation, WriteVcdAnnotation}
import chiseltest.legacy.backends.verilator.VerilatorFlags
import chiseltest.{ChiselScalatestTester, testableClock, testableData}
import firrtl.stage.RunFirrtlTransformAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import xiangshan.frontend.{Predictor, _}
import xiangshan.{DebugOptions, DebugOptionsKey, XSCoreParameters, XSCoreParamsKey}

import scala.collection.mutable.{Queue, Seq}
import scala.math.log10
import scala.util.control.Breaks._
import scala.collection.mutable.Map

case class FtqUpdate(clock_cnt: Long, resp_pc: Long, hit: Boolean, ghist: GlobalHistory, rasSp: BigInt, rasTop: RASEntry, specCnt: BigInt, meta:BigInt, taken_pc: Long, taken_target: Long, is_jal: Boolean, is_jalr: Boolean, is_call: Boolean, is_ret: Boolean)

case class FtqRedirect(clock_cnt: Long, pc: Long, interrupt: Boolean)

object RespRelations extends Enumeration {
    type RespRelation = Value
    val RespNone, RespMis, RespRightNoFollow, RespRightFollow = Value
}

trait HasBPUTestConst {
    val DebugEnable = true
    val latency: Int = 5
    val trace: String = "/Users/cgk/ownCloud/课程/ACS/XiangShan/src/test/scala/bp/test_trace.csv"
}

class BPTest extends AnyFlatSpec with ChiselScalatestTester with Matchers with HasBPUTestConst {
    // behavior of "Testers2"
    
    it should "test Generic Branch Prediction Unit" in {
        implicit val p = Parameters((site, up, here) => {
            case XSCoreParamsKey =>
                XSCoreParameters()
            case DebugOptionsKey =>
                DebugOptions()
        })
        val coreParams = p(XSCoreParamsKey)
        val FetchWidth = coreParams.FetchWidth
        val numBr = 2
        
        // init trace related files
        val file = io.Source.fromFile(trace)
        val file_buffer = file.getLines()
        var clock_count: Long = 0 // trace the clock so that we can provide redirect/update at given latency
        var first_time = true
        var got_resp = false
        var sent_update = false
        var sent_redirect = false
        var ftqUpdateQueue = Queue.empty[FtqUpdate]
        var ftqRedirectQueue = Queue.empty[FtqRedirect]
        def BranchPredictionToType(is_call: Boolean, is_jal: Boolean, is_jalr: Boolean, is_ret: Boolean, taken: Boolean) = {
            if (is_call && is_jal) {
                2 // jal call
            } else if (is_call && is_jalr) {
                3 // jalr call
            } else if (is_ret && is_jalr) {
                4 // jalr ret
            } else if (is_jal) {
                5 // oridinary jal(j)
            } else if (taken) {
                1 // branch
            } else {
                0 // not taken
            }
        }
        def TraceToType(c: Int) = {
            if (c == 0) {
                1
            } else if (c == 1) {
                2
            } else if (c == 2) {
                5
            } else if (c == 3) {
                4
            } else if (c == 4) {
                3
            }
        }
        def IntToPos(c: BigInt) = {
            if (c == 0) {
                16
            } else {
                Math.round(log10(c.doubleValue()) / log10(2.0))
            }
        }
        
        test(new FakeBPUWrapper()).withAnnotations(Seq(WriteVcdAnnotation))
          .withAnnotations(Seq(VerilatorBackendAnnotation,
              LineCoverageAnnotation,
              ToggleCoverageAnnotation,
              VerilatorFlags(Seq("--output-split 5000", "--output-split-cfuncs 5000",
                  "+define+RANDOMIZE_REG_INIT", "+define+RANDOMIZE_MEM_INIT")))) { c =>
              // do not generate any redirect by default
              c.io.ftq_to_bpu.redirect.valid.poke(0.B)
              c.io.ftq_to_bpu.update.valid.poke(0.B)
              
              var br_set:Map[Long, Long] = Map()
              
              var branch_info_line = file_buffer.next()
              // split string to numbers
              var branch_info_arr = branch_info_line.split(" ")
              // FIXME: 64bit PC may suffer
              var branch_pc = Integer.parseInt(branch_info_arr(0), 16)
              var branch_type = TraceToType(Integer.parseInt(branch_info_arr(1)))
              var branch_target = Integer.parseInt(branch_info_arr(2), 16)
              var branch_taken = Integer.parseInt(branch_info_arr(3))
              
              // redirect PC to the beginning of the trace
              c.io.ftq_to_bpu.redirect.valid.poke(1.B)
              c.io.ftq_to_bpu.redirect.bits.cfiUpdate.target.poke(branch_target.U)
              c.clock.step()
              c.io.ftq_to_bpu.redirect.valid.poke(0.B) // pull down
              clock_count = clock_count + 1
              def read_trace() = {
                  if (file_buffer.hasNext) {
                      branch_info_line = file_buffer.next()
                      // split string to numbers
                      branch_info_arr = branch_info_line.split(" ")
                      // FIXME: 64bit PC may suffer
                      branch_pc = Integer.parseInt(branch_info_arr(0), 16)
                      branch_type = TraceToType(Integer.parseInt(branch_info_arr(1)))
                      branch_target = Integer.parseInt(branch_info_arr(2), 16)
                      branch_taken = Integer.parseInt(branch_info_arr(3))
                      // record br target to set
                      if (branch_type == 1) {
                          // if is a branch
                          br_set += (branch_pc.toLong -> branch_taken.toLong)
                      }
                  }
              }
              
              read_trace()
              c.io.bpu_to_ftq.resp.ready.poke(1.B) // always happy to accept resp
              if (DebugEnable) {
                  println("Redirect done, Start test")
              }
              // we should check the output of BPU until we are done with the trace file
              while (file_buffer.hasNext) {
                  while (!got_resp && file_buffer.hasNext) {
                      // check if we have got an resp
                      val resp_valid = c.io.bpu_to_ftq.resp.valid.peek().litToBoolean
                      if (resp_valid) {
                          // got an resp, made a hand shake
                          val resp_pc = c.io.bpu_to_ftq.resp.bits.pc.peek()
                          val resp_hit = c.io.bpu_to_ftq.resp.bits.hit.peek()
                          val resp_branchIsJal = c.io.bpu_to_ftq.resp.bits.preds.is_jal.peek().litToBoolean
                          val resp_branchIsJalr = c.io.bpu_to_ftq.resp.bits.preds.is_jalr.peek().litToBoolean
                          val resp_branchIsCall = c.io.bpu_to_ftq.resp.bits.preds.is_call.peek().litToBoolean
                          val resp_branchIsRet = c.io.bpu_to_ftq.resp.bits.preds.is_ret.peek().litToBoolean
                          val resp_branchTarget = c.io.bpu_to_ftq.resp.bits.preds.target.peek()
                          val resp_branchMask = c.io.bpu_to_ftq.resp.bits.preds.taken_mask.peek().litValue()
                          val resp_branchTaken = resp_branchMask != 0
                          val resp_ghist = c.io.bpu_to_ftq.resp.bits.ghist.peek()
                          val resp_rasSp = c.io.bpu_to_ftq.resp.bits.rasSp.peek().litValue()
                          val resp_rasTop = c.io.bpu_to_ftq.resp.bits.rasTop.peek()
                          val resp_specCnt = c.io.bpu_to_ftq.resp.bits.specCnt.peek().litValue()
                          val resp_meta: BigInt = c.io.bpu_to_ftq.resp.bits.meta.peek().litValue
                          // FIXME: we do not check other infos from BPU resp
                          
                          // check if resp right
                          val actual_resp_pc = resp_pc.litValue() + IntToPos(resp_branchMask) * 2
                          val actual_resp_br_type = BranchPredictionToType(resp_branchIsCall, resp_branchIsJal, resp_branchIsJalr, resp_branchIsRet, resp_branchTaken)
                          val actual_resp_br_target = resp_branchTarget.litValue()
                          
                          val resp_pc_lit = resp_pc.litValue()
                          if (DebugEnable) {
                              println(f"Got an resp: resp_pc = 0x$resp_pc_lit%x, taken = $resp_branchTaken, br_target = 0x$actual_resp_br_target%x")
                          }
                          
                          def trace_relation(): RespRelation = {
                              if (branch_taken.equals(1)) {
                                  // branch taken
                                  if (!resp_hit.litToBoolean) {
                                      if (resp_pc.litValue() <= branch_pc && branch_pc < resp_pc.litValue() + FetchWidth * 4) {
                                          RespMis
                                      } else {
                                          RespNone
                                      }
                                  } else {
                                      //if (!(actual_resp_pc == branch_pc)) {
                                      //    RespMis
                                      //} else {
                                      if (actual_resp_br_target == branch_target) {
                                          RespRightNoFollow
                                      } else {
                                          RespMis
                                      }
                                      //}
                                  }
                              } else {
                                  // branch not taken
                                  /*
                                  if (!resp_hit.litToBoolean) {
                                      if (resp_pc.litValue() <= branch_pc && branch_pc < resp_pc.litValue() + FetchWidth * 4) {
                                          RespRightFollow
                                      } else {
                                          RespNone
                                      }
                                  } else if (actual_resp_pc > branch_pc) {
                                      RespRightFollow
                                  } else {
                                      RespMis
                                  }
                                   */
                                  RespRightFollow
                              }
                          }
                          // actually check the trace
                          var check_done = false
                          while (!check_done && file_buffer.hasNext) {
                              val res = trace_relation()
                              res match {
                                  case RespNone => {
                                      // end the loop but keep this trace
                                      if (DebugEnable) {
                                          println(f"Dont care this resp, expected addr 0x$branch_pc%x, expected br_target 0x$branch_target%x, actual addr 0x$actual_resp_pc%x, actual br_target 0x$actual_resp_br_target%x")
                                      }
                                      check_done = true
                                  }
                                  case RespMis => {
                                      // send redirect, end the loop but keep this trace
                                      if (DebugEnable) {
                                          println(f"Mispred resp, expected addr 0x$branch_pc%x, expected br_target 0x$branch_target%x, actual addr 0x$actual_resp_pc%x, actual br_target 0x$actual_resp_br_target%x")
                                          println(f"sending redirect, target $branch_target%x")
                                      }
                                      check_done = true
                                      // redirect needs to be sent right now
                                      c.io.ftq_to_bpu.redirect.valid.poke(1.B)
                                      c.io.ftq_to_bpu.redirect.bits.cfiUpdate.target.poke(branch_target.asUInt())
                                      val ftqUpdateEntry = FtqUpdate(clock_count + latency, resp_pc_lit.longValue(), hit = false, resp_ghist, resp_rasSp, resp_rasTop, resp_specCnt, resp_meta, branch_pc, branch_target, resp_branchIsJal, resp_branchIsJalr, resp_branchIsCall, resp_branchIsRet)
                                      ftqUpdateQueue.enqueue(ftqUpdateEntry)
                                      read_trace()
                                  }
                                  case RespRightNoFollow => {
                                      // reload trace, end the loop, record update
                                      if (DebugEnable) {
                                          println(f"Right taken resp, expected addr 0x$branch_pc%x, expected br_target 0x$branch_target%x, actual addr 0x$actual_resp_pc%x, actual br_target 0x$actual_resp_br_target%x")
                                      }
                                      // FIXME: we do not update non-taken branches
                                      check_done = true
                                      read_trace()
                                  }
                                  case RespRightFollow => {
                                      // reload trace, does not end the loop, record update
                                      if (DebugEnable) {
                                          println(f"Right non-taken resp, expected addr 0x$branch_pc%x, expected br_target 0x$branch_target%x, actual addr 0x$actual_resp_pc%x, actual br_target 0x$actual_resp_br_target%x")
                                      }
                                      //val ftqUpdateEntry = FtqUpdate(clock_count + latency, resp_pc_lit.longValue(), hit = false, resp_ghist, resp_rasSp, resp_rasTop, resp_specCnt, resp_meta, branch_pc, branch_target, resp_branchIsJal, resp_branchIsJalr, resp_branchIsCall, resp_branchIsRet)
                                      //ftqUpdateQueue.enqueue(ftqUpdateEntry)
                                      read_trace()
                                  }
                              }
                          }
                          
                          
                      }
                      // step one cycle ahead
                      c.clock.step()
                      clock_count = clock_count + 1
                      println(f"current clock $clock_count")
                      // any possible update/redirect only last for one cycle
                      c.io.ftq_to_bpu.update.valid.poke(0.B)
                      c.io.ftq_to_bpu.redirect.valid.poke(0.B)
                      // check if we have an update in queue
                      // note that now we only generate partial infos for FakeBPU
                      if (ftqUpdateQueue.nonEmpty) {
                          val ftqUpdate = ftqUpdateQueue.front
                          if (ftqUpdate.clock_cnt <= clock_count) {
                              // reached timeout, dequque and send an update request
                              ftqUpdateQueue.dequeue()
                              // TODO: send an update
                              sent_update = true
                              
                              if (DebugEnable) {
                                  println("Sending update")
                              }
                              
                              c.io.ftq_to_bpu.update.valid.poke(1.B)
                              
                              // TODO: add more logic here
                              // c.io.ftq_to_bpu.update.bits.
                              c.io.ftq_to_bpu.update.bits.mispred_mask.poke(0.U) // unset for now
                              c.io.ftq_to_bpu.update.bits.false_hit.poke(0.B)
                              c.io.ftq_to_bpu.update.bits.new_br_insert_pos.poke(0.U)
                              
                              // BranchPredictionBundle contents
                              c.io.ftq_to_bpu.update.bits.pc.poke(ftqUpdate.resp_pc.U)
                              c.io.ftq_to_bpu.update.bits.hit.poke(ftqUpdate.hit.B)
                              c.io.ftq_to_bpu.update.bits.ghist.poke(chiselTypeOf(c.io.ftq_to_bpu.update.bits.ghist).Lit(_.predHist -> ftqUpdate.ghist.predHist))
                              c.io.ftq_to_bpu.update.bits.rasSp.poke(ftqUpdate.rasSp.U)
                              c.io.ftq_to_bpu.update.bits.rasTop.poke(chiselTypeOf(c.io.ftq_to_bpu.update.bits.rasTop).Lit(_.retAddr -> ftqUpdate.rasTop.retAddr, _.ctr -> ftqUpdate.rasTop.ctr))
                              c.io.ftq_to_bpu.update.bits.specCnt.poke(ftqUpdate.specCnt.U)
                              c.io.ftq_to_bpu.update.bits.meta.poke(ftqUpdate.meta.U)
                              // do not set ftb related
                              c.io.ftq_to_bpu.update.bits.ftb_entry.jmpTarget.poke(ftqUpdate.taken_target.U)
                              
                              
                              c.io.ftq_to_bpu.update.bits.preds.is_jal.poke(ftqUpdate.is_jal.B)
                              c.io.ftq_to_bpu.update.bits.preds.is_jalr.poke(ftqUpdate.is_jalr.B)
                              c.io.ftq_to_bpu.update.bits.preds.is_call.poke(ftqUpdate.is_call.B)
                              c.io.ftq_to_bpu.update.bits.preds.is_ret.poke(ftqUpdate.is_ret.B)
                              c.io.ftq_to_bpu.update.bits.preds.call_is_rvc.poke(0.B) // dont care for now
                              c.io.ftq_to_bpu.update.bits.preds.target.poke(ftqUpdate.taken_target.U)
                              c.io.ftq_to_bpu.update.bits.preds.hit.poke(ftqUpdate.hit.B)
                              
                              // generate taken_mask and is_br
                              var is_br: Long = 0
                              var taken_mask: Long = 0
                              var br_count = 0
                              var i = 0
                              if (ftqUpdate.is_jal || ftqUpdate.is_jalr) {
                                  taken_mask = 1
                              }
                              while (i < FetchWidth * 2 && br_count < numBr) {
                                  val pc = ftqUpdate.resp_pc + 2 * i
                                  if (br_set.contains(pc)) {
                                      is_br = is_br | (1L << (numBr - br_count))
                                      if (br_set(pc) == 1) {
                                          taken_mask = taken_mask | (1L << (numBr - br_count))
                                      }
                                      br_count += 1
                                  }
                                  
                                  i += 1
                              }
                              c.io.ftq_to_bpu.update.bits.preds.is_br.poke(br_count.U)
                              c.io.ftq_to_bpu.update.bits.preds.taken_mask.poke(taken_mask.U)
                              
                              
                              
                          }
                      }
                  }
              }
          }
    }
}
