package Work

import Chisel._
import Node._
import scala.collection.mutable.HashMap
import scala.math._
import LMSConstants._
import ComplexMathFunctions._


// Helper Functions. These functions generate new hardware every time that they are called.
object ComplexMathFunctions
{
    // Function to perform a complex multiply
    def complex_mult(x: ComplexSFix, y: ComplexSFix): ComplexSFix =
    {
        val z = new ComplexSFix(w=x.width)
        z.real := x.real * y.real - x.imag * y.imag
        z.imag := x.real * y.imag + x.imag * y.real
        return z
    }

    // Function to compute dot-product of two vectors
    def dot(vecA: Vec[ComplexSFix], vecB: Vec[ComplexSFix], vecLen: Int): ComplexSFix =
    {
        val result = new ComplexSFix(w=vecA(0).width)

        for (i <- 0 until vecLen)
            result := result + complex_mult( vecA(i), vecB(i) )

        return result
    }
}


class MatrixEngineIO(implicit params: LMSParams) extends Bundle()
{
    // Matrix input port
    // There is a separate port for each matrix row, and each of these ports
    // contains a sub-port for each element in that row. Therefore, for
    // a 4x4 matrix, 16 values are passed in a single clock cycle
    val matrixIn = Vec.fill(params.max_ntx_nrx){
                    Vec.fill(params.max_ntx_nrx){new ComplexSFix(w=params.fix_pt_wd)}}

    // The vector input port
    val vectorIn = Vec.fill(params.max_ntx_nrx){new ComplexSFix(w=params.fix_pt_wd)}

    // Matrix (or vector) output port
    // Clock out a column on each clock cycle
    val result = Vec.fill(params.max_ntx_nrx){new ComplexSFix(w=params.fix_pt_wd)}
}


// Module to estimate the channel matrix H
class MatrixEngine(implicit params: LMSParams) extends Module
{
    val io = new MatrixEngineIO()

    // Call dot once for each row of matrix
    // Each result is a different element in the output Vec
    // If the input matrices/vectors are smaller than the maximum size,
    // the computation is still done.
    for (i <- 0 until params.max_ntx_nrx)
    {
        io.result(i) := Reg(next = dot(io.matrixIn(i), io.vectorIn, params.max_ntx_nrx))
    }
}



