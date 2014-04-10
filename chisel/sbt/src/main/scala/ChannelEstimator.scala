package Work

import Chisel._
import Node._
import scala.collection.mutable.HashMap
import scala.math._
//import LMSDefines._
import LMSConstants._

class ChannelEstimatorIO(implicit params: LMSParams) extends Bundle()
{
}


// Module to estimate the channel matrix H
class ChannelEstimator(implicit params: LMSParams) extends Module
{
    val io = new ChannelEstimatorIO()
}


