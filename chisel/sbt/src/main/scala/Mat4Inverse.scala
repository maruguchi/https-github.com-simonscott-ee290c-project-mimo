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
	val matIn = Vec.fill(4){ Vec.fill(4) {new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp).asInput } }

	val matOut = Vec.fill(4){ Vec.fill(4) {new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp).asOutput } }
}

class Mat4Inverse (implicit params:LMSParams) extends Module
{
	val io = new Mat4InverseIO()

	// local matrix inversion hardware (reused)
	val mat2inverse = Module(new Mat2Inverse())

	// block matrices in input
	val A = Vec.fill(2){ Vec.fill(2){ new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp)}}
	val B = Vec.fill(2){ Vec.fill(2){ new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp)}}
	val C = Vec.fill(2){ Vec.fill(2){ new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp)}}
	val D = Vec.fill(2){ Vec.fill(2){ new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp)}}

	// fill the initial block matrices
	for (i <- 0 until 2) {
		for (j <- 0 until 2) {
			A(i)(j) := io.matIn(i)(j)
			B(i)(j) := io.matIn(i+2)(j)
			C(i)(j) := io.matIn(i)(j+2)
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

	CAinv := mat2multiply( C, Ainv )
	AinvB := mat2multiply( Ainv, B )
	schur := mat2subtract( D, mat2multiply(CAinv,B) )
	AinvBschurInv := mat2multiply( AinvB, schurInv )

	// keeps track of which step, sends appropriate matrices to inversion block
	val step = Reg(init = UInt(0,1))

	when (step === UInt(0,1)) {
		mat2inverse.io.matIn := A
		Ainv := mat2inverse.io.matOut
		step := UInt(1,1)
	} .otherwise {
		mat2inverse.io.matIn := schur
		schurInv := mat2inverse.io.matOut
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
			io.matOut(i+2)(j) := Bfinal(i)(j)
			io.matOut(i)(j+2) := Cfinal(i)(j)
			io.matOut(i+2)(j+2) := Dfinal(i)(j)
		}
	}

}

class Mat4InverseTests(c: Mat4Inverse, params: LMSParams) extends Tester(c)
{
val matIn_r = Array( Array(1.3,-2.1,1.2,1), Array(0.3,-0.5,-0.4,-1), Array(0.2,0.5,-0.9,1), Array(0.1,0.95,-0.3,-1))
val matIn_i = Array( Array(1.1,0.7,2.1,1), Array(-1.1,1.1,0.3,1), Array(-0.3,0.5,-0.7,1), Array(-0.3,0.5,-0.7,1))

for (t <- 0 until 1)
    {
        // Apply inputs
	for (i <- 0 until 4) {
		for (j <- 0 until 4) {
			poke(c.io.matIn(i)(j).real.raw, conv_double_to_fp(matIn_r(i)(j), params.fix_pt_frac_bits, params.fix_pt_wd))
			poke(c.io.matIn(i)(j).imag.raw, conv_double_to_fp(matIn_i(i)(j), params.fix_pt_frac_bits, params.fix_pt_wd))
		}
	}

        // Clock the module
        step(6)

	println()
	println()
	for (i <- 0 until 4) {
		for (j <- 0 until 4) {
			println( conv_fp_to_double(peek(c.io.matOut(i)(j).real.raw), params.fix_pt_frac_bits, params.fix_pt_wd) )
			println( conv_fp_to_double(peek(c.io.matOut(i)(j).imag.raw), params.fix_pt_frac_bits, params.fix_pt_wd) )
		}
	}	
    }
}
