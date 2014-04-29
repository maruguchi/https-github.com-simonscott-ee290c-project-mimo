package Work

import Chisel._
import Node._
import scala.collection.mutable.HashMap
import scala.math._
import LMSConstants._
import ComplexMathFunctions._
import FixedPoint._

class FixTestIO(implicit params: LMSParams) extends Bundle()
{
	val in = new ComplexSFix(w=params.samp_wd, e=params.samp_int_bits).asInput
	val out = new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp).asOutput

}

class FixTest (implicit params:LMSParams) extends Module
{
	val io = new FixTestIO()
	//val three = makeComplexSFix(w=params.fix_pt_wd, r=12288, i=0)

	io.out := io.in
}

class FixTestTests(c: FixTest, params: LMSParams) extends Tester(c)
{
val in_r = conv_double_to_samp(4, params.samp_int_bits, params.samp_wd)
val in_i = 0
val result_r = 1
val result_i = 0

for (t <- 0 until 1)
    {
        // Apply inputs
        poke(c.io.in.real.raw, in_r)
        poke(c.io.in.imag.raw, in_i)

        // Clock the module
        step(1)

        // Check the output
	expect(c.io.out.real.raw, result_r)
	expect(c.io.out.imag.raw, result_i)
	val y = peek(c.io.out.real.raw)

	print( conv_fp_to_double(y, params.fix_pt_frac_bits, params.fix_pt_wd) )
    	println()
    }

}
