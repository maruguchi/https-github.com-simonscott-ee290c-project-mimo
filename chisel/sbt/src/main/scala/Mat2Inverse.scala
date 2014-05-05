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

	val rst = Bool().asInput
	val done = Bool().asOutput
}

class Mat2Inverse (implicit params:LMSParams) extends Module
{
	val io = new Mat2InverseIO()

	val divider = Module(new FixDivision())

	// determinant
	val det = Reg(new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp))
	det := complex_sub(complex_mult(io.matIn(0)(0), io.matIn(1)(1)), complex_mult(io.matIn(0)(1), io.matIn(1)(0)))

	// value of zero for negation
	val zero = makeComplexSFix(w=params.fix_pt_wd, r=0, i=0)

	val out00 = Reg(init = zero)
	val out01 = Reg(init = zero)
	val out10 = Reg(init = zero)
	val out11 = Reg(init = zero)

	val counter = Reg(init = UInt(0,3))
	val process_data = (counter < UInt(6)) && (~io.rst)
	val done = (counter === UInt(6))

	val x1 = complex_sub(zero, io.matIn(0)(1))
	val x2 = complex_sub(zero, io.matIn(1)(0))

	when (io.rst) {
		counter := UInt(0)
	}
	when (process_data) {
		counter := counter + UInt(1)
	}

	divider.io.den := det

	when (process_data && ~done && counter === UInt(1)) {
		divider.io.num := io.matIn(1)(1)
	} .elsewhen (process_data && ~done && counter === UInt(2)) {
		divider.io.num := io.matIn(0)(0)
		out00 := divider.io.result
	} .elsewhen (process_data && ~done && counter === UInt(3)) {
		divider.io.num := x1
		out11 := divider.io.result
	} .elsewhen (process_data && ~done && counter === UInt(4)) {
		divider.io.num := x2
		out01 := divider.io.result
	} .elsewhen (process_data && ~done && counter === UInt(5)) {
		divider.io.num := x2
		out10 := divider.io.result
	} .otherwise {
		divider.io.num := zero
	}

	io.matOut(0)(0) := out00
	io.matOut(0)(1) := out01
	io.matOut(1)(0) := out10
	io.matOut(1)(1) := out11
	io.done := done
}

class Mat2InverseTests(c: Mat2Inverse, params: LMSParams) extends Tester(c)
{
val matIn_r = Array( Array(15.7,1.15), Array(1.15,5.06))
val matIn_i = Array( Array(0,0.52), Array(-0.52,0))

for (t <- 0 until 1)
    {
	poke(c.io.rst,0)
        // Apply inputs
	for (i <- 0 until 2) {
		for (j <- 0 until 2) {
			poke(c.io.matIn(i)(j).real.raw, conv_double_to_fp(matIn_r(i)(j), params.fix_pt_frac_bits, params.fix_pt_wd))
			poke(c.io.matIn(i)(j).imag.raw, conv_double_to_fp(matIn_i(i)(j), params.fix_pt_frac_bits, params.fix_pt_wd))
		}
	}
	step(7)

        // Clock the module
        
        // Check the output
	for (i <- 0 until 2) {
		for (j <- 0 until 2) {
			print( conv_fp_to_double(peek(c.io.matOut(i)(j).real.raw), params.fix_pt_frac_bits, params.fix_pt_wd) )
			print( conv_fp_to_double(peek(c.io.matOut(i)(j).imag.raw), params.fix_pt_frac_bits, params.fix_pt_wd) )
		}
	}

    }
}
