package Work

import Chisel._
import Node._
import scala.collection.mutable.HashMap
import scala.math._
import LMSConstants._
import ComplexMathFunctions._
import FixedPoint._

class Mat4InverseIO(implicit params: LMSParams) extends Bundle()
{
	// input matrix to be inverted
	val matIn = Vec.fill(4){ Vec.fill(4) {new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp).asInput } }

	// output matrix which is the inverse
	val matOut = Vec.fill(4){ Vec.fill(4) {new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp).asOutput } }

	// resets internal counter (state machine) to zero
	val rst = Bool().asInput

	// IO buses to the inverse2 engine which this uses for computation
	val mat2inverse = new Mat2InverseIO().flip()
}

class Mat4Inverse (implicit params:LMSParams) extends Module
{
	val io = new Mat4InverseIO()

	// from matrix inversion module in higher level module
	val inverse_done = io.mat2inverse.done

	// block matrices in input
	val A = Vec.fill(2){ Vec.fill(2){ new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp)}}
	val B = Vec.fill(2){ Vec.fill(2){ new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp)}}
	val C = Vec.fill(2){ Vec.fill(2){ new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp)}}
	val D = Vec.fill(2){ Vec.fill(2){ new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp)}}

	// fill the initial block matrices
	for (i <- 0 until 2) {
		for (j <- 0 until 2) {
			A(i)(j) := io.matIn(i)(j)
			B(i)(j) := io.matIn(i)(j+2)
			C(i)(j) := io.matIn(i+2)(j)
			D(i)(j) := io.matIn(i+2)(j+2)
		}
	}

	// intermediate results
	val Ainv = Vec.fill(2){ Vec.fill(2){ Reg(init = makeComplexSFix(w=params.fix_pt_wd, r=0, i=0)) } }
	val CAinv = Vec.fill(2){ Vec.fill(2){ Reg(init = makeComplexSFix(w=params.fix_pt_wd, r=0, i=0))}}
	val AinvB = Vec.fill(2){ Vec.fill(2){ Reg(init = makeComplexSFix(w=params.fix_pt_wd, r=0, i=0))}}
	val schur = Vec.fill(2){ Vec.fill(2){ Reg(init = makeComplexSFix(w=params.fix_pt_wd, r=0, i=0))}}
	val schurInv = Vec.fill(2){ Vec.fill(2){ Reg(init = makeComplexSFix(w=params.fix_pt_wd, r=0, i=0))}}
	val AinvBschurInv = Vec.fill(2){ Vec.fill(2){ Reg(init = makeComplexSFix(w=params.fix_pt_wd, r=0, i=0))}}

	// logic for the intermediate products
	CAinv := mat2multiply( C, Ainv )
	AinvB := mat2multiply( Ainv, B )
	schur := mat2subtract( D, mat2multiply(CAinv,B) )
	AinvBschurInv := mat2multiply( AinvB, schurInv )

	// keeps track of which step, sends appropriate matrices to inversion block
	val step = Reg(init = UInt(0,5))

	when (io.rst) {
		step := UInt(0)
	}

	// kind of state machine controlling behavior of the module
	when (step === UInt(0)) {
		step := UInt(1)
		io.mat2inverse.rst := Bool(true)
	} .elsewhen (step === UInt(1)) {
		io.mat2inverse.rst := Bool(false)
		io.mat2inverse.matIn := A
		when (inverse_done) {
			io.mat2inverse.rst := Bool(true)
			Ainv := io.mat2inverse.matOut
			step := step + UInt(1)
		}
	} .otherwise {
		step := step + UInt(1)
		io.mat2inverse.rst := step === UInt(4)
		io.mat2inverse.matIn := schur
		when (inverse_done) {
			schurInv := io.mat2inverse.matOut
		}
	}

	// computes block matrices in inverse
	val Afinal = Vec.fill(2){ Vec.fill(2){ new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp)}}
	val Bfinal = Vec.fill(2){ Vec.fill(2){ new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp)}}
	val Cfinal = Vec.fill(2){ Vec.fill(2){ new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp)}}
	val Dfinal = Vec.fill(2){ Vec.fill(2){ new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp)}}

	Afinal := mat2add( Ainv, mat2multiply( AinvBschurInv, CAinv ) )
	Bfinal := mat2negate( AinvBschurInv )
	Cfinal := mat2negate( mat2multiply( schurInv, CAinv ) )
	Dfinal := schurInv

	// fills in the output from the blocks
	for (i <- 0 until 2) {
		for (j <- 0 until 2) {
			io.matOut(i)(j) := Afinal(i)(j)
			io.matOut(i)(j+2) := Bfinal(i)(j)
			io.matOut(i+2)(j) := Cfinal(i)(j)
			io.matOut(i+2)(j+2) := Dfinal(i)(j)
		}
	}

}

class Mat4InverseTests(c: Mat4Inverse, params: LMSParams) extends Tester(c)
{
val matIn_r = Array( Array(1.321,-0.372,-0.750,-0.08), Array(-0.372,1.07,0.035,0.197), Array(-0.750,0.035,1.158,0.265), Array(-0.080,0.197,0.265,0.138))
val matIn_i = Array( Array(0,0.217,0.725,0.332), Array(-0.217,0,0.286,0.064), Array(-0.725,-0.286,0,-0.259), Array(-0.332,-0.064,0.259,0))

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
	peek(c.io.mat2inverse.rst)
	step(1)
	peek(c.io.mat2inverse.rst)
	step(1)
	peek(c.io.mat2inverse.rst)
	step(11)

	for (i <- 0 until 4) {
		for (j <- 0 until 4) {
			println( conv_fp_to_double(peek(c.io.matOut(i)(j).real.raw), params.fix_pt_frac_bits, params.fix_pt_wd) )
			println( conv_fp_to_double(peek(c.io.matOut(i)(j).imag.raw), params.fix_pt_frac_bits, params.fix_pt_wd) )
		}
	}	

    }
}
