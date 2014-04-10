package Work

import Chisel._
import Node._
import scala.collection.mutable.HashMap
import scala.math._
import LMSConstants._

// Notes from discussion on April 10:
// For now, only support QPSK when deciding what symbol was sent
// Output is symbols (just use integers to represent each symbol,
// e.g. 0 for +1 +1j, 1 for +1 - 1j, 2 for ...)

class AdaptiveDecoderIO(implicit params: LMSParams) extends Bundle()
{
}


// Module to estimate the channel matrix H
class AdaptiveDecoder(implicit params: LMSParams) extends Module
{
    val io = new AdaptiveDecoderIO()
}


