package Work

import Chisel._
import Node._
import scala.collection.mutable.HashMap
import scala.math._
import LMSConstants._
import ComplexMathFunctions._
import FixedPoint._

class Mat3InverseIO(implicit params: LMSParams) extends Bundle()
{
	val matIn = Vec.fill(3){ Vec.fill(3) {new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp).asInput } }

	val matOut = Vec.fill(3){ Vec.fill(3) {new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp).asOutput } }
}

class Mat3Inverse (implicit params:LMSParams) extends Module
{
	val io = new Mat3InverseIO()

	// determinant
	val det1 = new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp)
	val det2 = new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp)
	val det3 = new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp)
	val prod1 = new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp)
	val prod2 = new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp)
	val prod3 = new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp)
	val det = new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp)

	det1 := complex_sub(complex_mult(io.matIn(1)(1), io.matIn(2)(2)), complex_mult(io.matIn(1)(2), io.matIn(2)(1)))
	det2 := complex_sub(complex_mult(io.matIn(0)(1), io.matIn(2)(2)), complex_mult(io.matIn(0)(2), io.matIn(2)(1)))
	det3 := complex_sub(complex_mult(io.matIn(0)(1), io.matIn(1)(2)), complex_mult(io.matIn(1)(1), io.matIn(0)(2)))

	prod1 := complex_mult(io.matIn(0)(0), det1)
	prod2 := complex_mult(io.matIn(1)(0), det2)
	prod3 := complex_mult(io.matIn(2)(0), det3)
	det := complex_sub( complex_add(prod1, prod3), prod2 ) 

	// value of zero for negation
	val zero = makeComplexSFix(w=params.fix_pt_wd, r=0, i=0)

	// compute inverse
	val result = Vec.fill(3){ Vec.fill(3) {new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp) } }

	result(0)(0) := complex_div(det1, det)
	result(0)(1) := complex_div(complex_sub(zero, det2), det)
	result(0)(2) := complex_div(det3, det)

	result(1)(0) := complex_div(complex_sub(complex_mult(io.matIn(2)(0),io.matIn(1)(2)), complex_mult(io.matIn(1)(0),io.matIn(2)(2))), det)
	result(1)(1) := complex_div(complex_sub(complex_mult(io.matIn(0)(0),io.matIn(2)(2)), complex_mult(io.matIn(2)(0),io.matIn(0)(2))), det)
	result(1)(2) := complex_div(complex_sub(complex_mult(io.matIn(1)(0),io.matIn(0)(2)), complex_mult(io.matIn(0)(0),io.matIn(1)(2))), det)

	result(2)(0) := complex_div(complex_sub(complex_mult(io.matIn(1)(0),io.matIn(2)(1)), complex_mult(io.matIn(2)(0),io.matIn(1)(1))), det)
	result(2)(1) := complex_div(complex_sub(complex_mult(io.matIn(2)(0),io.matIn(0)(1)), complex_mult(io.matIn(0)(0),io.matIn(2)(1))), det)
	result(2)(2) := complex_div(complex_sub(complex_mult(io.matIn(0)(0),io.matIn(1)(1)), complex_mult(io.matIn(1)(0),io.matIn(0)(1))), det)

	io.matOut := result
}

class Mat3InverseTests(c: Mat3Inverse, params: LMSParams) extends Tester(c)
{
val matIn_r = Array( Array(1.3,-2.1,1.2), Array(0.3,-0.5,-0.4), Array(0.2,0.5,-0.9))
val matIn_i = Array( Array(1.1,0.7,2.1), Array(-1.1,1.1,0.3), Array(-0.3,0.5,-0.7))

for (t <- 0 until 1)
    {
        // Apply inputs
	for (i <- 0 until 3) {
		for (j <- 0 until 3) {
			poke(c.io.matIn(i)(j).real.raw, conv_double_to_fp(matIn_r(i)(j), params.fix_pt_frac_bits, params.fix_pt_wd))
			poke(c.io.matIn(i)(j).imag.raw, conv_double_to_fp(matIn_i(i)(j), params.fix_pt_frac_bits, params.fix_pt_wd))
		}
	}

        // Clock the module
        step(1)

        // Check the output
	for (i <- 0 until 3) {
		for (j <- 0 until 3) {
			println( conv_fp_to_double(peek(c.io.matOut(i)(j).real.raw), params.fix_pt_frac_bits, params.fix_pt_wd) )
			println( conv_fp_to_double(peek(c.io.matOut(i)(j).imag.raw), params.fix_pt_frac_bits, params.fix_pt_wd) )
		}
	}

    }
}
