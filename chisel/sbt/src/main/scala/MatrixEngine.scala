package Work

import Chisel._
import Node._
import scala.collection.mutable.HashMap
import scala.math._
import LMSConstants._

// Notes from discussion on April 10:
// Support matrix-vec multiply, matrix-matrix multiply and conjugate transpose

class MatrixEngineIO(implicit params: LMSParams) extends Bundle()
{
}


// Module to estimate the channel matrix H
class MatrixEngine(implicit params: LMSParams) extends Module
{
    val io = new MatrixEngineIO()
}


