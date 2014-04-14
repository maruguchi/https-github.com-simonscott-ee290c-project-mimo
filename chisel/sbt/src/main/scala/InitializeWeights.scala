package Work

import Chisel._
import Node._
import scala.collection.mutable.HashMap
import scala.math._
import LMSConstants._

// Notes from discussion on April 10:
// This module computes W from the H estimate
// Use the matrix engine to perform the mat-mat multiplies
// Output is fixed point (probably around 32-bits wide)

class InitializeWeightsIO(implicit params: LMSParams) extends Bundle()
{
}


// Module to estimate the channel matrix H
class InitializeWeights(implicit params: LMSParams) extends Module
{
    val io = new InitializeWeightsIO()
}


