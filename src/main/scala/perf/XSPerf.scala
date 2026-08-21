package boom.perf

import chisel3._
import chisel3.experimental.StringParam
import chisel3.util.HasBlackBoxResource
import chisel3.util.experimental.BoringUtils.tapAndRead
import chisel3.reflect.DataMirror.isVisible
import org.chipsalliance.cde.config.{Config, Field, Parameters}

case object BoomXSPerfEnableKey extends Field[Boolean](false)

class LogPerfIO extends Bundle {
  val clock = Clock()
  val reset = Bool()
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
    require(perfName.matches("\\w+"), s"invalid BOOM xsperf name: $perfName")
    if (p(BoomXSPerfEnableKey)) {
      if (perfInfos.isEmpty) XSLog.registerCaller(collect)
      perfInfos += ((perfName, perfCnt))
    }
  }

  private def collect(ctrl: LogPerfIO)(implicit p: Parameters): Unit = {
    perfInfos.zipWithIndex.foreach { case ((name, sourceBore), index) =>
      // Collection happens at the core-level endpoint, while callers may live
      // in any BOOM submodule. Preserve XiangShan's deferred cross-module read.
      val source = if (isVisible(sourceBore)) sourceBore else tapAndRead(sourceBore)
      val counter = RegInit(0.U(64.W)).suggestName(s"${name}_xsperf_counter_$index")
      val next = counter + source
      counter := next

      val endpoint = Module(new XSPerfEndpoint(name)).suggestName(s"${name}_xsperf_endpoint_$index")
      endpoint.io.clock := ctrl.clock
      endpoint.io.reset := ctrl.reset
      endpoint.io.value := next
    }
  }
}

class XSPerfEndpoint(perfName: String) extends BlackBox(Map(
  "PERF_NAME" -> StringParam(perfName)
)) with HasBlackBoxResource {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val value = Input(UInt(64.W))
  })
  addResource("/boom-perf/XSPerfEndpoint.v")
}

/** Opt-in configuration shared by FDIP and coupled BOOMv3 perf runs. */
class WithBoomXSPerf extends Config((site, here, up) => {
  case BoomXSPerfEnableKey => true
})
