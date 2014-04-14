package Work

import Chisel._
import Node._
import scala.collection.mutable.HashMap
import scala.math._
import LMSConstants._

// New idea:
// To do the matrix-vec multiply in 1 cycle (ie. W*x, while decoding stream), make a dot-product module
// and instantiate it once for each row in W.


// Notes from discussion on April 10:
// Support matrix-vec multiply, matrix-matrix multiply and conjugate transpose

class MatrixEngineIO(implicit params: LMSParams) extends Bundle()
{
    // Matrix input port
    // There is a separate port for each matrix row, and each of these ports
    // contains a sub-port for each element in that row. Therefore, for
    // a 4x4 matrix, 16 values are passed in a single clock cycle
    val matrixIn = Vec.fill(max(params.max_ntx, params.max_nrx)){
                    Vec.fill(max(params.max_ntx, params.max_nrx)){new ComplexSFix(w=params.fix_pt_wd)}}

    // The vector input port
    val vectorIn = Vec.fill(max(params.max_ntx, params.max_nrx)){new ComplexSFix(w=params.fix_pt_wd)}

    // Matrix (or vector) output port
    // Clock out a column on each clock cycle
    val result = Vec.fill(max(params.max_ntx, params.max_nrx)){new ComplexSFix(w=params.fix_pt_wd)}

    // Input ports to specify the size of each input matrix
    val matrixRows = UInt()
    val matrixCols = UInt()
    val vectorLen = UInt()

    // Output port specifying the size of the resulting vector
    val resultLen = UInt()

    // validIn: hold this high while clocking in the matrix, vector and their sizes
    // The matrix operation starts as soon as validIn goes low
    // validOut: goes high as soon as the matrix computation is complete.
    val validIn = Bool()
    val validOut = Bool()
}


// Module to estimate the channel matrix H
class MatrixEngine(implicit params: LMSParams) extends Module
{
    val io = new MatrixEngineIO()

    // Function to compute dot-product of two vectors
    //def dot(vecA: ComplexSFix, vecB: ComplexSFix): ComplexSFix = {
        
    //}
}



