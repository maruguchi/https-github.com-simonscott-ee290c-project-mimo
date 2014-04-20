package Work

import Chisel._
import Node._
import scala.collection.mutable.HashMap
import scala.math._
import LMSConstants._
import ComplexMathFunctions._
import FixedPoint._


// Helper Functions. These functions generate new hardware every time that they are called.
object ComplexMathFunctions
{
    // Function to perform a complex multiply
    def complex_mult(x: ComplexSFix, y: ComplexSFix): ComplexSFix =
    {
        val z = new ComplexSFix(w=x.real.raw.width, e=x.real.raw.width)
        val minus_1 = new SFix(exp = x.real.raw.width, raw = SInt(-1))

        z.real := (x.real * y.real) + (x.imag * y.imag * minus_1)
        z.imag := x.real * y.imag + x.imag * y.real
        return z
    }

    // Function to add two complex numbers
    def complex_add(x: ComplexSFix, y: ComplexSFix): ComplexSFix =
    {
        val result_wd = max(x.real.raw.width, y.real.raw.width)
        val result = makeComplexSFix(w = result_wd, r = 0, i = 0)
        result.real := x.real + y.real
        result.imag := x.imag + y.imag
        return result
    }

    // Function to compute dot-product of two vectors
    def dot(vecA: Vec[ComplexSFix], vecB: Vec[ComplexSFix], vecLen: Int): ComplexSFix =
    {
        val result_wd = max(vecA(0).real.raw.width, vecB(0).real.raw.width)
        val result = makeComplexSFix(w = result_wd, r = 0, i = 0)
        val mult_out = Vec.fill(vecLen){new ComplexSFix(w=result_wd)}

        for (i <- 0 until vecLen)
            mult_out(i) := complex_mult( vecA(i), vecB(i) )

        result := mult_out.reduceLeft(complex_add)
        return result
    }

    // Function to create a new complex signed fix-pt value and initialize it
    def makeComplexSFix(w: Int, r: Int, i: Int): ComplexSFix =
    {
        val x = new ComplexSFix(w = w, e = w)
        x.real.raw := SInt(r, width=w)
        x.imag.raw := SInt(i, width=w)
        return x 
    }
}


class MatrixEngineIO(implicit params: LMSParams) extends Bundle()
{
    // Matrix input port
    // There is a separate port for each matrix row, and each of these ports
    // contains a sub-port for each element in that row. Therefore, for
    // a 4x4 matrix, 16 values are passed in a single clock cycle
    val matrixIn = Vec.fill(params.max_ntx_nrx){
                    Vec.fill(params.max_ntx_nrx){new ComplexSFix(w=params.fix_pt_wd).asInput}}

    // The vector input port
    val vectorIn = Vec.fill(params.max_ntx_nrx){new ComplexSFix(w=params.fix_pt_wd).asInput}

    // Matrix (or vector) output port
    // Clock out a column on each clock cycle
    val result = Vec.fill(params.max_ntx_nrx){new ComplexSFix(w=params.fix_pt_wd).asOutput}
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


// Tester for testing the matrix engine
class MatrixEngineTests(c: MatrixEngine, params: LMSParams) extends Tester(c)
{
    val test_matrix_in_r = Array( Array(1,2,3,4), Array(11,12,13,14), Array(21,22,23,24), Array(31,32,33,34) )
    val test_matrix_in_i = Array( Array(0,0,0,0), Array(0,0,0,0), Array(0,0,0,0), Array(0,0,0,0) )
    val test_vec_in_r = Array(1, 0, 0, 0)
    val test_vec_in_i = Array(0, 0, 0, 0)
    val test_vec_out_r = Array(1, 11, 21, 31)
    val test_vec_out_i = Array(0, 0, 0, 0)

    for (t <- 0 until 1)
    {
        // Apply inputs
        for(i <- 0 until params.max_ntx_nrx) {
            for(j <- 0 until params.max_ntx_nrx) {
                poke(c.io.matrixIn(i)(j).real.raw, test_matrix_in_r(i)(j))
                poke(c.io.matrixIn(i)(j).imag.raw, test_matrix_in_i(i)(j))
            }
            poke(c.io.vectorIn(i).real.raw, test_vec_in_r(i))
            poke(c.io.vectorIn(i).imag.raw, test_vec_in_i(i))
        }

        // Clock the module
        step(1)
        step(1)

        // Check the output
        for(i <- 0 until params.max_ntx_nrx) {
            expect(c.io.result(i).real.raw, test_vec_out_r(i))
            expect(c.io.result(i).imag.raw, test_vec_out_i(i))
        }
    }
}


