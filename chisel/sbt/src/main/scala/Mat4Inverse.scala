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

	// IO buses to matrix engine for matrix multiplications
	val toMatEngine = new MatrixEngineIO().flip()
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

	val zero = makeComplexSFix(w=params.fix_pt_wd, r=0, i=0)

	// intermediate results
	val Ainv = Vec.fill(2){ Vec.fill(2){ Reg(init = zero) } }
	val CAinv = Vec.fill(2){ Vec.fill(2){ Reg(init = zero) } }
	val CAinvB = Vec.fill(2){ Vec.fill(2){ Reg(init = zero) } }
	val AinvB = Vec.fill(2){ Vec.fill(2){ Reg(init = zero) } }
	val schur = Vec.fill(2){ Vec.fill(2){ Reg(init = zero) } }
	val schurInv = Vec.fill(2){ Vec.fill(2){ Reg(init = zero) } }
	val AinvBschurInv = Vec.fill(2){ Vec.fill(2){ Reg(init = zero) } }
	val schurInvCAinv = Vec.fill(2){ Vec.fill(2){ Reg(init = zero) } }
	val AinvBschurInvCAinv = Vec.fill(2){ Vec.fill(2){ Reg(init = zero) } }

	// keeps track of which step, sends appropriate matrices to inversion block
	val step = Reg(init = UInt(0,5))

	// keeps track of matrix engine latency
	val engine_counter = Reg(init = UInt(0,3))

	when (io.rst) {
		step := UInt(0)
	}

	for (i <- 2 until params.max_ntx_nrx ) {
		for (j <- 0 until params.max_ntx_nrx) {
			io.toMatEngine.matrixIn(i)(j) := zero
		}
		io.toMatEngine.vectorIn(i) := zero
	}

	for (i <- 0 until 2) {
		for (j <- 2 until params.max_ntx_nrx) {
			io.toMatEngine.matrixIn(i)(j) := zero
		}
	}

	val x1 = mat2subtract( D, CAinvB )

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
	} .elsewhen (step === UInt(2)) {
		engine_counter := engine_counter + UInt(1)

		for (i <- 0 until 2) {
			for (j <- 0 until 2) {
				io.toMatEngine.matrixIn(i)(j) := C(i)(j)
			}
		}

		when (engine_counter === UInt(0)) {
			io.toMatEngine.vectorIn(0) := Ainv(0)(0)
			io.toMatEngine.vectorIn(1) := Ainv(1)(0)
		} .elsewhen (engine_counter === UInt(1)) {
			io.toMatEngine.vectorIn(0) := Ainv(0)(1)
			io.toMatEngine.vectorIn(1) := Ainv(1)(1)
			CAinv(0)(0) := io.toMatEngine.result(0)
			CAinv(1)(0) := io.toMatEngine.result(1)
		} .otherwise {
			CAinv(0)(1) := io.toMatEngine.result(0)
			CAinv(1)(1) := io.toMatEngine.result(1)
			engine_counter := UInt(0)
			step := UInt(3)
		}

	} .elsewhen (step === UInt(3)) {
		engine_counter := engine_counter + UInt(1)

		for (i <- 0 until 2) {
			for (j <- 0 until 2) {
				io.toMatEngine.matrixIn(i)(j) := CAinv(i)(j)
			}
		}

		when (engine_counter === UInt(0)) {
			io.toMatEngine.vectorIn(0) := B(0)(0)
			io.toMatEngine.vectorIn(1) := B(1)(0)
		} .elsewhen (engine_counter === UInt(1)) {
			io.toMatEngine.vectorIn(0) := B(0)(1)
			io.toMatEngine.vectorIn(1) := B(1)(1)
			CAinvB(0)(0) := io.toMatEngine.result(0)
			CAinvB(1)(0) := io.toMatEngine.result(1)
		} .elsewhen (engine_counter === UInt(2)) {
			CAinvB(0)(1) := io.toMatEngine.result(0)
			CAinvB(1)(1) := io.toMatEngine.result(1)
		} .otherwise {
			engine_counter := UInt(0)
			step := UInt(4)
			io.mat2inverse.rst := Bool(true)
		}
		
		schur := x1

	} .elsewhen (step === UInt(4)) {
		io.mat2inverse.rst := Bool(false)
		io.mat2inverse.matIn := schur
		when (inverse_done) {
			schurInv := io.mat2inverse.matOut
			step := UInt(5)
			engine_counter := UInt(0)
		}

		for (i <- 0 until 2) {
			for (j <- 0 until 2) {
				io.toMatEngine.matrixIn(i)(j) := Ainv(i)(j)
			}
		}

		when (engine_counter === UInt(0)) {
			engine_counter := UInt(1)
			io.toMatEngine.vectorIn(0) := B(0)(0)
			io.toMatEngine.vectorIn(1) := B(1)(0)
		} .elsewhen (engine_counter === UInt(1)) {
			engine_counter := UInt(2)
			io.toMatEngine.vectorIn(0) := B(0)(1)
			io.toMatEngine.vectorIn(1) := B(1)(1)
			AinvB(0)(0) := io.toMatEngine.result(0)
			AinvB(1)(0) := io.toMatEngine.result(1)
		} .otherwise {
			io.toMatEngine.vectorIn(0) := B(0)(1)
			io.toMatEngine.vectorIn(1) := B(1)(1)
			AinvB(0)(1) := io.toMatEngine.result(0)
			AinvB(1)(1) := io.toMatEngine.result(1)
		} 

	} .elsewhen (step === UInt(5)) {
		engine_counter := engine_counter + UInt(1)

		for (i <- 0 until 2) {
			for (j <- 0 until 2) {
				io.toMatEngine.matrixIn(i)(j) := AinvB(i)(j)
			}
		}

		when (engine_counter === UInt(0)) {
			io.toMatEngine.vectorIn(0) := schurInv(0)(0)
			io.toMatEngine.vectorIn(1) := schurInv(1)(0)
		} .elsewhen (engine_counter === UInt(1)) {
			io.toMatEngine.vectorIn(0) := schurInv(0)(1)
			io.toMatEngine.vectorIn(1) := schurInv(1)(1)
			AinvBschurInv(0)(0) := io.toMatEngine.result(0)
			AinvBschurInv(1)(0) := io.toMatEngine.result(1)
		} .otherwise {
			AinvBschurInv(0)(1) := io.toMatEngine.result(0)
			AinvBschurInv(1)(1) := io.toMatEngine.result(1)
			engine_counter := UInt(0)
			step := UInt(6)
		}
	} .elsewhen (step === UInt(6)) {
		engine_counter := engine_counter + UInt(1)

		for (i <- 0 until 2) {
			for (j <- 0 until 2) {
				io.toMatEngine.matrixIn(i)(j) := schurInv(i)(j)
			}
		}

		when (engine_counter === UInt(0)) {
			io.toMatEngine.vectorIn(0) := CAinv(0)(0)
			io.toMatEngine.vectorIn(1) := CAinv(1)(0)
		} .elsewhen (engine_counter === UInt(1)) {
			io.toMatEngine.vectorIn(0) := CAinv(0)(1)
			io.toMatEngine.vectorIn(1) := CAinv(1)(1)
			schurInvCAinv(0)(0) := io.toMatEngine.result(0)
			schurInvCAinv(1)(0) := io.toMatEngine.result(1)
		} .otherwise {
			schurInvCAinv(0)(1) := io.toMatEngine.result(0)
			schurInvCAinv(1)(1) := io.toMatEngine.result(1)
			engine_counter := UInt(0)
			step := UInt(7)
		}
	} .elsewhen (step === UInt(7)) {
		engine_counter := engine_counter + UInt(1)

		for (i <- 0 until 2) {
			for (j <- 0 until 2) {
				io.toMatEngine.matrixIn(i)(j) := AinvBschurInv(i)(j)
			}
		}

		when (engine_counter === UInt(0)) {
			io.toMatEngine.vectorIn(0) := CAinv(0)(0)
			io.toMatEngine.vectorIn(1) := CAinv(1)(0)
		} .elsewhen (engine_counter === UInt(1)) {
			io.toMatEngine.vectorIn(0) := CAinv(0)(1)
			io.toMatEngine.vectorIn(1) := CAinv(1)(1)
			AinvBschurInvCAinv(0)(0) := io.toMatEngine.result(0)
			AinvBschurInvCAinv(1)(0) := io.toMatEngine.result(1)
		} .otherwise {
			AinvBschurInvCAinv(0)(1) := io.toMatEngine.result(0)
			AinvBschurInvCAinv(1)(1) := io.toMatEngine.result(1)
			engine_counter := UInt(0)
			step := UInt(8)
		}
	} .otherwise {
		io.toMatEngine.matrixIn(0)(0) := zero
		io.toMatEngine.matrixIn(0)(1) := zero
		io.toMatEngine.matrixIn(1)(0) := zero
		io.toMatEngine.matrixIn(1)(1) := zero
		io.toMatEngine.vectorIn(0) := zero
		io.toMatEngine.vectorIn(1) := zero

		io.mat2inverse.rst := Bool(true)
		io.mat2inverse.matIn := Vec.fill(2){ Vec.fill(2){ zero } }
		
		Ainv := Ainv
		CAinv := CAinv
		CAinvB := CAinvB
		schur := schur
		schurInv := schurInv
		AinvB := AinvB
		AinvBschurInv := AinvBschurInv
		schurInvCAinv := schurInvCAinv
		AinvBschurInvCAinv := AinvBschurInvCAinv
	}

	// computes block matrices in inverse
	val Afinal = Vec.fill(2){ Vec.fill(2){ new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp) } }
	val Bfinal = Vec.fill(2){ Vec.fill(2){ new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp) } }
	val Cfinal = Vec.fill(2){ Vec.fill(2){ new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp) } }
	val Dfinal = Vec.fill(2){ Vec.fill(2){ new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp) } }

	Afinal := mat2add( Ainv, AinvBschurInvCAinv )
	Bfinal := mat2negate( AinvBschurInv )
	Cfinal := mat2negate( schurInvCAinv )
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
