package boom.fdip.ifu.bpd.ras

import org.chipsalliance.cde.config.Parameters

import boom.fdip.ifu.bpd.{BpdBundle, BpdModule}

abstract class RasBundle(implicit p: Parameters) extends BpdBundle()(p) with HasRasParameters

abstract class RasModule(implicit p: Parameters) extends BpdModule()(p) with HasRasParameters
