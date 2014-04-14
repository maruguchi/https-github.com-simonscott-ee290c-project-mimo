package Work

import Chisel._
import Node._
import scala.collection.mutable.HashMap
import scala.math._
import LMSConstants._

// Notes from discussion on April 10:
// H should be signed integers at the output (multiply weights by 4)
// These integers will probably be 16 bits wide, as samples are 10 bit
// Also, rewrite the MATLAB code to perform this as mat-vec multiply.
// The mat-vec multiply can be done using the matrix engine

class ChannelEstimatorIO(implicit params: LMSParams) extends Bundle()
{
}


// Module to estimate the channel matrix H
class ChannelEstimator(implicit params: LMSParams) extends Module
{
    val io = new ChannelEstimatorIO()
}


