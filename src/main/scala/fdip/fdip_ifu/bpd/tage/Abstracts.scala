package boom.fdip.ifu.bpd.tage

import org.chipsalliance.cde.config.Parameters

import boom.fdip.ifu.bpd.{BpdBundle, BpdModule}

abstract class TageBundle(implicit p: Parameters) extends BpdBundle()(p) with HasTageParameters

abstract class TageModule(implicit p: Parameters) extends BpdModule()(p) with HasTageParameters
