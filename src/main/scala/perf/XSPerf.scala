package boom.perf

import chisel3._
import chisel3.util.HasBlackBoxResource
import org.chipsalliance.cde.config.{Config, Field, Parameters}

case object BoomXSPerfEnableKey extends Field[Boolean](false)

class LogPerfIO extends Bundle {
  val clock = Clock()
  val reset = Bool()
}

private object XSPerfNames {
  val all = Seq(
    "cycles", "commit_instr", "branch_resolved", "branch_mispredict",
    "control_flow_target_mispredict", "flush", "icache_demand_miss",
    "dcache_miss", "dtlb_miss", "l2tlb_miss", "icache_prefetch",
    "ftq_full", "ftq_metadata_conflict", "ftq_redirect_conflict")
}

object XSLog {
  private val callbacks = scala.collection.mutable.ListBuffer.empty[LogPerfIO => Unit]
  def registerCaller(caller: LogPerfIO => Unit): Unit = callbacks += caller
  def invokeCaller(ctrl: LogPerfIO): Unit = callbacks.foreach(_(ctrl))
  def collect(ctrl: LogPerfIO)(implicit p: Parameters): Unit = invokeCaller(ctrl)
}

/** BOOM port of XiangShan's XSPerfAccumulate deferred registration API. */
object XSPerfAccumulate {
  private val perfInfos = scala.collection.mutable.ListBuffer.empty[(String, UInt)]

  def apply(perfName: String, perfCnt: UInt)(implicit p: Parameters): Unit = {
    require(XSPerfNames.all.contains(perfName), s"unknown BOOM xsperf name: $perfName")
    if (p(BoomXSPerfEnableKey)) {
      if (perfInfos.isEmpty) XSLog.registerCaller(collect)
      perfInfos += ((perfName, perfCnt))
    }
  }

  private def collect(ctrl: LogPerfIO)(implicit p: Parameters): Unit = {
    val counters = XSPerfNames.all.map { name =>
      val source = perfInfos.find(_._1 == name).map(_._2).getOrElse(0.U)
      val counter = RegInit(0.U(64.W)).suggestName(s"${name}_xsperf_counter")
      val next = counter + source
      counter := next
      next
    }

    val endpoint = Module(new XSPerfEndpoint)
    endpoint.io.clock := ctrl.clock
    endpoint.io.reset := ctrl.reset
    endpoint.io.values.zip(counters).foreach { case (out, value) => out := value }
  }
}

class XSPerfEndpoint extends BlackBox with HasBlackBoxResource {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val values = Input(Vec(XSPerfNames.all.size, UInt(64.W)))
  })
  addResource("/boom-perf/XSPerfEndpoint.v")
}

/** Opt-in configuration shared by FDIP and coupled BOOMv3 perf runs. */
class WithBoomXSPerf extends Config((site, here, up) => {
  case BoomXSPerfEnableKey => true
})
