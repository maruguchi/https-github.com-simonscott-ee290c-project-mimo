package Work

import Chisel._
import Node._
import scala.collection.mutable.HashMap
import scala.math._
import LMSConstants._
import ComplexMathFunctions._
import FixedPoint._

class Mat4InverseEngineIO(implicit params: LMSParams) extends Bundle()
{
	// input matrix to be inverted
	val matIn = Vec.fill(4){ Vec.fill(4) {new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp).asInput } }

	// output matrix which is the inverse
	val matOut = Vec.fill(4){ Vec.fill(4) {new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp).asOutput } }

	// resets internal counter (state machine) to zero
	val rst = Bool().asInput
}

class Mat4InverseEngine (implicit params:LMSParams) extends Module
{
	val io = new Mat4InverseEngineIO()

	val inverse4 = Module(new Mat4Inverse())
	val engine = Module(new MatrixEngine())
	val inverse2 = Module(new Mat2Inverse())

	inverse4.io.mat2inverse <> inverse2.io
	inverse4.io.toMatEngine <> engine.io

	inverse4.io.matIn := io.matIn
	inverse4.io.rst := io.rst
	
	io.matOut := inverse4.io.matOut

}

class Mat4InverseTests(c: Mat4InverseEngine, params: LMSParams) extends Tester(c)
{
val matIn_r = Array( Array(1.3, -2.1, 1.2, 1), Array(0.3, -0.5, -0.4, -1), Array(0.2, 0.5, -0.9, 1), Array(0.1, 0.95, -0.3, -1))
val matIn_i = Array( Array(1.1, 0.7, 2.1, 1), Array(-1.1, 1.1, 0.3, 1), Array(-0.3, 0.5, -0.7, 1), Array(-0.3, 0.5, -0.7, 1))

for (t <- 0 until 1)
    {
	poke(c.io.rst,0)
        // Apply inputs
	for (i <- 0 until 4) {
		for (j <- 0 until 4) {
			poke(c.io.matIn(i)(j).real.raw, conv_double_to_fp(matIn_r(i)(j), params.fix_pt_frac_bits, params.fix_pt_wd))
			poke(c.io.matIn(i)(j).imag.raw, conv_double_to_fp(matIn_i(i)(j), params.fix_pt_frac_bits, params.fix_pt_wd))
		}
	}

        // Clock the module
        step(10)
	peek(c.inverse2.io.rst)
	step(1)
	peek(c.inverse2.io.rst)
	step(1)
	peek(c.inverse2.io.rst)
	step(20)

	for (i <- 0 until 4) {
		for (j <- 0 until 4) {
			println( conv_fp_to_double(peek(c.io.matOut(i)(j).real.raw), params.fix_pt_frac_bits, params.fix_pt_wd) )
			println( conv_fp_to_double(peek(c.io.matOut(i)(j).imag.raw), params.fix_pt_frac_bits, params.fix_pt_wd) )
		}
	}	

    }
}
