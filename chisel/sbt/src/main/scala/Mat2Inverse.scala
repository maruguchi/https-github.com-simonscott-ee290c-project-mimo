package Work

import Chisel._
import Node._
import scala.collection.mutable.HashMap
import scala.math._
import LMSConstants._
import ComplexMathFunctions._
import FixedPoint._

class Mat2InverseIO(implicit params: LMSParams) extends Bundle()
{
	val matIn = Vec.fill(2){ Vec.fill(2) {new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp).asInput } }

	val matOut = Vec.fill(2){ Vec.fill(2) {new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp).asOutput } }

	val det = new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp).asOutput
}

class Mat2Inverse (implicit params:LMSParams) extends Module
{
	val io = new Mat2InverseIO()

	val det = new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp)
	det := complex_sub(complex_mult(io.matIn(0)(0), io.matIn(1)(1)), complex_mult(io.matIn(0)(1), io.matIn(1)(0)))

	val zero = makeComplexSFix(w=params.fix_pt_wd, r=0, i=0)

	val result = Vec.fill(2){ Vec.fill(2) {new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp) } }
	result(0)(0) := complex_div(io.matIn(1)(1), det)
	result(1)(1) := complex_div(io.matIn(0)(0), det)
	result(0)(1) := complex_div(complex_sub(zero, io.matIn(1)(0)), det)
	result(1)(0) := complex_div(complex_sub(zero, io.matIn(0)(1)), det)

	io.matOut := result
	io.det := det

}

class Mat2InverseTests(c: Mat2Inverse, params: LMSParams) extends Tester(c)
{
val matIn_r = Array( Array(1.3,-2.1), Array(0.3,-0.5))
val matIn_i = Array( Array(1.1,0.7), Array(-1.1,1.1))

for (t <- 0 until 1)
    {
        // Apply inputs
	for (i <- 0 until 2) {
		for (j <- 0 until 2) {
			poke(c.io.matIn(i)(j).real.raw, conv_double_to_fp(matIn_r(i)(j), params.fix_pt_frac_bits, params.fix_pt_wd))
			poke(c.io.matIn(i)(j).imag.raw, conv_double_to_fp(matIn_i(i)(j), params.fix_pt_frac_bits, params.fix_pt_wd))
		}
	}

        // Clock the module
        step(1)

        // Check the output
	for (i <- 0 until 2) {
		for (j <- 0 until 2) {
			print( conv_fp_to_double(peek(c.io.matOut(i)(j).real.raw), params.fix_pt_frac_bits, params.fix_pt_wd) )
			print( conv_fp_to_double(peek(c.io.matOut(i)(j).imag.raw), params.fix_pt_frac_bits, params.fix_pt_wd) )
		}
	}
	print( conv_fp_to_double(peek(c.io.det.real.raw), params.fix_pt_frac_bits, params.fix_pt_wd) )
	print( conv_fp_to_double(peek(c.io.det.imag.raw), params.fix_pt_frac_bits, params.fix_pt_wd) )

//	print( conv_fp_to_double(yr, params.fix_pt_frac_bits, params.fix_pt_wd) )
//	println()
//	print( conv_fp_to_double(yi, params.fix_pt_frac_bits, params.fix_pt_wd) )
//	println()
    }
}
