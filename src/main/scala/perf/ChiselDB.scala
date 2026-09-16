package boom.perf

import chisel3._
import chisel3.experimental.StringParam
import chisel3.util.{Cat, DecoupledIO, HasBlackBoxInline, Valid}
import org.chipsalliance.cde.config.{Config, Field, Parameters}

case object BoomChiselDBEnableKey extends Field[Boolean](false)

/**
  * Simulator-only SQLite traces, adapted from XiangShan ChiselDB.
  *
  * Build a configuration containing [[WithBoomChiselDB]] with
  * `ENABLE_BOOM_CHISELDB=1`, then pass `+chisel_db=/path/to/trace.db` to the
  * simulator. Each Chisel Record becomes a SQLite table with flattened,
  * individually queryable columns plus STAMP and SITE.
  */
object ChiselDB {
  def createTable[T <: Record](name: String, hw: T)(implicit p: Parameters): ChiselDBTable[T] = {
    require(name.matches("[A-Za-z][A-Za-z0-9_]*"), s"invalid ChiselDB table name: $name")
    // Keep enable/type state local to this elaboration and hart. The runtime
    // checks schema compatibility when multiple writers share a table name.
    new ChiselDBTable[T](name, hw, p(BoomChiselDBEnableKey))
  }
}

class ChiselDBTable[T <: Record](val name: String, val hw: T, enabled: Boolean) {
  private def columns(data: Data, prefix: String = ""): Seq[(String, UInt)] = data match {
    case record: Record => record.elements.toSeq.flatMap { case (field, value) =>
      columns(value, if (prefix.isEmpty) field else s"${prefix}_$field")
    }
    case vector: Vec[_] => vector.zipWithIndex.flatMap { case (value, index) =>
      columns(value, s"${prefix}_$index")
    }
    case element: Element if element.widthKnown =>
      val bits = element.asUInt
      (0 until element.getWidth by 64).map { low =>
        val field = if (element.getWidth <= 64) prefix else s"${prefix}_${low / 64}"
        (field, bits(math.min(low + 63, element.getWidth - 1), low))
      }
    case element: Element => throw new IllegalArgumentException(s"unknown ChiselDB width: ${element.instanceName}")
  }

  def log(data: T, en: Bool, site: String = "")(implicit clock: Clock, reset: Reset): Unit = {
    if (enabled) {
      val fields = columns(data)
      require(fields.nonEmpty, s"empty ChiselDB record: $name")
      val names = fields.map(_._1.toUpperCase(java.util.Locale.ROOT))
      require(names.distinct.size == names.size && !names.exists(Set("ID", "STAMP", "SITE")),
        s"duplicate or reserved ChiselDB field in $name")
      val schema = fields.map { case (field, value) => s"$field:${value.getWidth}" }.mkString(",")
      val writer = Module(new ChiselDBWriter(name, fields.map(_._2.getWidth).sum, schema, site))
      writer.io.clock := clock
      writer.io.reset := reset
      writer.io.en := en
      // Schema order is explicitly MSB first, independent of Bundle/Vec asUInt ordering.
      writer.io.data := Cat(fields.map(_._2))
    }
  }

  def log(data: Valid[T], site: String)(implicit clock: Clock, reset: Reset): Unit =
    log(data.bits, data.valid, site)

  def log(data: DecoupledIO[T], site: String)(implicit clock: Clock, reset: Reset): Unit =
    log(data.bits, data.fire, site)
}

private class ChiselDBWriter(tableName: String, dataWidth: Int, schema: String, site: String)
    extends BlackBox(Map(
      "TABLE_NAME" -> StringParam(tableName),
      "SCHEMA" -> StringParam(schema),
      "SITE" -> StringParam(site)
    )) with HasBlackBoxInline {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Reset())
    val en = Input(Bool())
    val data = Input(UInt(dataWidth.W))
  })

  override def desiredName: String = s"ChiselDBWriter_${tableName}_$dataWidth"

  setInline(s"${desiredName}.sv",
    s"""module ${desiredName}(
       |  input clock,
       |  input reset,
       |  input en,
       |  input [${dataWidth - 1}:0] data
       |);
       |  import "DPI-C" function void chisel_db_enable(input string path);
       |  import "DPI-C" function void chisel_db_write(
       |    input string table_name, input string schema, input longint unsigned stamp,
       |    input string site, input string payload);
       |  import "DPI-C" function void chisel_db_save();
       |  parameter string TABLE_NAME = "";
       |  parameter string SCHEMA = "";
       |  parameter string SITE = "";
       |  longint unsigned stamp = 64'd0;
       |  bit enabled = 1'b0;
       |  string db_path;
       |
       |  initial begin
       |    enabled = $$value$$plusargs("chisel_db=%s", db_path) != 0;
       |    if (enabled) chisel_db_enable(db_path);
       |  end
       |
       |  always @(posedge clock) begin
       |    if (reset) stamp <= 64'd0;
       |    else begin
       |      stamp <= stamp + 1;
       |      if (enabled && en) chisel_db_write(TABLE_NAME, SCHEMA, stamp, SITE, $$sformatf("%h", data));
       |    end
       |  end
       |
       |  final chisel_db_save();
       |endmodule
       |""".stripMargin)
}

class WithBoomChiselDB extends Config((site, here, up) => {
  case BoomChiselDBEnableKey => true
})
