package boom.fdip.ifu.bpd

import org.chipsalliance.cde.config.Parameters

import boom.fdip.common.{BoomBundle, BoomModule}

abstract class PHRBundle(implicit p: Parameters) extends BoomBundle()(p) with HasBpdParameters

abstract class PHRModule(implicit p: Parameters) extends BoomModule()(p) with HasBpdParameters
