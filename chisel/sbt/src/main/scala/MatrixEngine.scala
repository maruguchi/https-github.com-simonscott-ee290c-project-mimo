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
    // Function to compute a complex conjugate
    def conj(x: ComplexSFix)(implicit params: LMSParams): ComplexSFix =
    {
        val z = new ComplexSFix(w=x.real.raw.width, e=x.real.exp)

        z.real := x.real
        z.imag := -x.imag
        return z
    }

    // Function to perform a complex multiply
    def complex_mult(x: ComplexSFix, y: ComplexSFix)(implicit params: LMSParams): ComplexSFix =
    {
        val z = new ComplexSFix(w=x.real.raw.width, e=x.real.exp)

        z.real := (x.real * y.real) - (x.imag * y.imag)
        z.imag := x.real * y.imag + x.imag * y.real
        return z
    }

    // Function to perform a complex division
    def complex_div(x: ComplexSFix, y: ComplexSFix)(implicit params: LMSParams): ComplexSFix = 
    {
	val z = new ComplexSFix(w=x.real.raw.width, e=x.real.exp)
	val prod = new ComplexSFix(w=x.real.raw.width, e=x.real.exp)

	prod := complex_mult(x, conj(y))

	val den_mag = SFix(exp = params.fix_pt_exp, width = params.fix_pt_wd)
	den_mag := y.real * y.real + y.imag * y.imag

	z.real.raw := (prod.real.raw << UInt(params.fix_pt_wd - params.fix_pt_exp)) / den_mag.raw
	z.imag.raw := (prod.imag.raw << UInt(params.fix_pt_wd - params.fix_pt_exp)) / den_mag.raw
	return z
    }

    // Function to add two complex numbers
    def complex_add(x: ComplexSFix, y: ComplexSFix)(implicit params: LMSParams): ComplexSFix =
    {
        val result_wd = max(x.real.raw.width, y.real.raw.width)
        val result = makeComplexSFix(w = result_wd, r = 0, i = 0)
        result.real := x.real + y.real
        result.imag := x.imag + y.imag
        return result
    }

    // Function to subtract two complex numbers: x - y
    def complex_sub(x: ComplexSFix, y: ComplexSFix)(implicit params: LMSParams): ComplexSFix =
    {
        val result_wd = max(x.real.raw.width, y.real.raw.width)
        val result = makeComplexSFix(w = result_wd, r = 0, i = 0)
        result.real := x.real - y.real
        result.imag := x.imag - y.imag
        return result
    }

    // Function to compute dot-product of two vectors
    def dot(vecA: Vec[ComplexSFix], vecB: Vec[ComplexSFix], vecLen: Int)(implicit params: LMSParams): ComplexSFix =
    {
        val result_wd = max(vecA(0).real.raw.width, vecB(0).real.raw.width)
        val result = makeComplexSFix(w = result_wd, r = 0, i = 0)
        val mult_out = Vec.fill(vecLen){new ComplexSFix(w=result_wd, e=vecA(0).real.exp)}

        for (i <- 0 until vecLen)
            mult_out(i) := complex_mult( vecA(i), vecB(i) )

        result := mult_out.reduceLeft(complex_add)

        return result
    }

    // Function to negate 2x2 matrices
    def mat2negate(A: Vec[Vec[ComplexSFix]])(implicit params: LMSParams): Vec[Vec[ComplexSFix]] =
    {
        val matOut = Vec.fill(2){Vec.fill(2){ new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp) } }

	val zero = makeComplexSFix(w=params.fix_pt_wd, r=0, i=0)

	matOut(0)(0) := complex_sub( zero, A(0)(0) )
	matOut(1)(0) := complex_sub( zero, A(1)(0) )
	matOut(0)(1) := complex_sub( zero, A(0)(1) )
	matOut(1)(1) := complex_sub( zero, A(1)(1) )

        return matOut
    }

    // Function to compute the sum of 2x2 matrices
    def mat2add(A: Vec[Vec[ComplexSFix]], B: Vec[Vec[ComplexSFix]])(implicit params: LMSParams): Vec[Vec[ComplexSFix]] =
    {
        val matOut = Vec.fill(2){Vec.fill(2){ new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp) } }

	matOut(0)(0) := complex_add( A(0)(0), B(0)(0) )
	matOut(1)(0) := complex_add( A(1)(0), B(1)(0) )
	matOut(0)(1) := complex_add( A(0)(1), B(0)(1) )
	matOut(1)(1) := complex_add( A(1)(1), B(1)(1) )

        return matOut
    }

    // Function to compute the difference of 2x2 matrices
    def mat2subtract(A: Vec[Vec[ComplexSFix]], B: Vec[Vec[ComplexSFix]])(implicit params: LMSParams): Vec[Vec[ComplexSFix]] =
    {
        val matOut = Vec.fill(2){Vec.fill(2){ new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp) } }

	matOut(0)(0) := complex_sub( A(0)(0), B(0)(0) )
	matOut(1)(0) := complex_sub( A(1)(0), B(1)(0) )
	matOut(0)(1) := complex_sub( A(0)(1), B(0)(1) )
	matOut(1)(1) := complex_sub( A(1)(1), B(1)(1) )

        return matOut
    }

    // Function to compute a product of 2x2 matrices
    def mat2multiply(A: Vec[Vec[ComplexSFix]], B: Vec[Vec[ComplexSFix]])(implicit params: LMSParams): Vec[Vec[ComplexSFix]] =
    {
        val matOut = Vec.fill(2){Vec.fill(2){ new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp) } }

	matOut(0)(0) := complex_add( complex_mult( A(0)(0), B(0)(0) ), complex_mult( A(1)(0), B(0)(1) ) )
	matOut(1)(0) := complex_add( complex_mult( A(0)(0), B(1)(0) ), complex_mult( A(1)(0), B(1)(1) ) )
	matOut(0)(1) := complex_add( complex_mult( A(0)(1), B(0)(0) ), complex_mult( A(1)(1), B(0)(1) ) )
	matOut(1)(1) := complex_add( complex_mult( A(0)(1), B(1)(0) ), complex_mult( A(1)(1), B(1)(1) ) )

        return matOut
    }

    // Function to create a new complex signed fix-pt value and initialize it
    def makeComplexSFix(w: Int, r: Int, i: Int)(implicit params: LMSParams): ComplexSFix =
    {
        val x = new ComplexSFix(w = w, e = params.fix_pt_exp)
        x.real.raw := SInt(r, width=w)
        x.imag.raw := SInt(i, width=w)
        return x 
    }

    // Function to convert double numbers to fixed point
    def conv_double_to_fp(d: Double, fp_frac_bits: Int, fp_bit_wd: Int): Int = 
    {
        var fp = (d * pow(2, fp_frac_bits)).toInt
        fp = if(fp < 0) ( pow(2, fp_bit_wd).toInt + fp ) else fp
        return fp
    }

    // Function to convert fixed point numbers to doubles
    def conv_fp_to_double(fp: BigInt, fp_frac_bits: Int, fp_bit_wd: Int): Double = 
    {
        var d = if(fp > pow(2, fp_bit_wd-1).toInt) (fp.toDouble - pow(2, fp_bit_wd)) else fp.toDouble
        d = d * pow(2, -fp_frac_bits)
        return d
    }

    // Function to convert double numbers to integer samples
    def conv_double_to_samp(d: Double, samp_int_bits: Int, samp_bit_wd: Int): Int = 
    {
        var samp = (d * pow(2, samp_bit_wd - samp_int_bits)).toInt
        samp = if(samp < 0) ( pow(2, samp_bit_wd).toInt + samp ) else samp
        return samp
    }
}


class MatrixEngineIO(implicit params: LMSParams) extends Bundle()
{
    // Matrix input port
    // There is a separate port for each matrix row, and each of these ports
    // contains a sub-port for each element in that row. Therefore, for
    // a 4x4 matrix, 16 values are passed in a single clock cycle
    val matrixIn = Vec.fill(params.max_ntx_nrx){
                    Vec.fill(params.max_ntx_nrx){new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp).asInput}}

    // The vector input port
    val vectorIn = Vec.fill(params.max_ntx_nrx){new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp).asInput}

    // Matrix (or vector) output port
    // Clock out a column on each clock cycle
    val result = Vec.fill(params.max_ntx_nrx){new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp).asOutput}
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
/*
    val test_matrix_in_r = Array( Array(1.3,2.0,3.0,4.0), Array(11.0,12.0,13.0,14.0), Array(21.0,22.0,23.0,24.0), Array(31.0,32.0,33.0,34.0) )
    val test_matrix_in_i = Array( Array(0,0,0,0), Array(0,0,0,0), Array(0,0,0,0), Array(0,0,0,0) )
    val test_vec_in_r = Array(1, 0, 0, 0)
    val test_vec_in_i = Array(0, 0, 0, 0)
    val test_vec_out_r = Array(1.3, 11.0, 21.0, 31.0)
    val test_vec_out_i = Array(0, 0, 0, 0)
*/
    val test_matrix_in_r = Array( Array(4.33, 2.40, 4.51, 2.93 ), Array(3.07, 4.01, 2.88, 1.24 ), Array(4.95, 1.14, 4.23, 3.34 ), Array(2.64, 2.50, 3.70, 0.42 ) )
    val test_matrix_in_i = Array( Array(3.13, 4.92, 2.91, 2.43 ), Array(3.31, 3.85, 0.09, 4.23 ), Array(3.65, 2.91, 0.61, 1.05 ), Array(4.46, 4.65, 4.32, 2.77 ) )
    val test_vec_in_r = Array(3.15, 0.16, 3.08, 1.82  )
    val test_vec_in_i = Array(0.25, 2.45, 0.97, 0.62  )
    val test_vec_out_r = Array(16.08, 8.47, 25.60, 2.46  )
    val test_vec_out_i = Array(37.19, 33.17, 25.96, 43.77  )

    val threshold = 0.01

    for (t <- 0 until 1)
    {
        // Apply inputs
        for(i <- 0 until params.max_ntx_nrx) {
            for(j <- 0 until params.max_ntx_nrx) {
                poke(c.io.matrixIn(i)(j).real.raw, ((test_matrix_in_r(i)(j)).toDouble * pow(2, params.fix_pt_frac_bits)).toInt)
                poke(c.io.matrixIn(i)(j).imag.raw, (test_matrix_in_i(i)(j) * pow(2, params.fix_pt_frac_bits)).toInt)
            }
            poke(c.io.vectorIn(i).real.raw, (test_vec_in_r(i) * pow(2, params.fix_pt_frac_bits)).toInt)
            poke(c.io.vectorIn(i).imag.raw, (test_vec_in_i(i) * pow(2, params.fix_pt_frac_bits)).toInt)
        }

        // Clock the module
        step(1)
        step(1)

        // Check the output
        for(i <- 0 until params.max_ntx_nrx) {
            var real_comp = ((if(peek(c.io.result(i).real.raw) >= pow(2, params.fix_pt_wd-1).toInt) (pow(2, params.fix_pt_wd).toInt - peek(c.io.result(i).real.raw)) else
                            (peek(c.io.result(i).real.raw)))).toDouble * pow(2, -params.fix_pt_frac_bits)
            var imag_comp = ((if(peek(c.io.result(i).imag.raw) >= pow(2, params.fix_pt_wd-1).toInt) (pow(2, params.fix_pt_wd).toInt - peek(c.io.result(i).imag.raw)) else
                            (peek(c.io.result(i).imag.raw)))).toDouble * pow(2, -params.fix_pt_frac_bits)

            expect( abs(real_comp - test_vec_out_r(i)) < threshold, s"Checking real value. Expect: ${test_vec_out_r(i)}. Got: $real_comp" )
            expect( abs(imag_comp - test_vec_out_i(i)) < threshold, s"Checking imag value. Expect: ${test_vec_out_i(i)}. Got: $imag_comp" )
        }
    }
}


