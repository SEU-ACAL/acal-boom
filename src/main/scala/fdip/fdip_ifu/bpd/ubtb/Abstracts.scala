package boom.fdip.ifu.bpd.ubtb

import org.chipsalliance.cde.config.Parameters

import boom.fdip.ifu.bpd.{BpdBundle, BpdModule}

abstract class UbtbBundle(implicit p: Parameters) extends BpdBundle()(p) with HasUbtbParameters

abstract class UbtbModule(implicit p: Parameters) extends BpdModule()(p) with HasUbtbParameters
