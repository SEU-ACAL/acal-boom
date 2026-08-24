package boom.fdip.ifu.bpd.l1btb

import org.chipsalliance.cde.config.Parameters

import boom.fdip.ifu.bpd.{BpdBundle, BpdModule}

abstract class L1BtbBundle(implicit p: Parameters) extends BpdBundle()(p) with HasL1BtbParameters

abstract class L1BtbModule(implicit p: Parameters) extends BpdModule()(p) with HasL1BtbParameters
