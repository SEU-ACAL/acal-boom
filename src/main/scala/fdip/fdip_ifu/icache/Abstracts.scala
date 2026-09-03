package boom.fdip.ifu

import org.chipsalliance.cde.config.Parameters

import boom.fdip.common.{BoomBundle, BoomModule}

abstract class ICacheBundle(implicit p: Parameters) extends BoomBundle()(p) with HasICacheParameters

abstract class ICacheModule(implicit p: Parameters) extends BoomModule()(p) with HasICacheParameters
