package Work

import Chisel._
import Node._
import scala.collection.mutable.HashMap
import scala.math._
import LMSConstants._
import ComplexMathFunctions._
import FixedPoint._

// Notes from discussion on April 10:
// This module computes W from the H estimate
// Use the matrix engine to perform the mat-mat multiplies
// Output is fixed point (probably around 32-bits wide)

class InitializeWeightsIO(implicit params: LMSParams) extends Bundle()
{
	// value telling the module to being computation (could come from channelEsitmator io.done)
	val start = Bool().asInput

	// value to reset the module
	val rst = Bool().asInput
	
	// channel matrix input
	val channelMatrix = Vec.fill(params.max_ntx_nrx){ 
		Vec.fill(params.max_ntx_nrx) {new ComplexSFix(w=params.fix_pt_wd, e = params.fix_pt_exp).asInput } }
	
	// Number of Tx/Rx antennas
	val Nant = UInt(width = REG_WD).asInput

	// SNR (linear)
	val snr = UInt(width=REG_WD).asInput

	// MatrixEngine IO for multiplying matrices
	val toMatEngine = new MatrixEngineIO().flip()

	// output matrix with initial W seed
	val initialW = Vec.fill(params.max_ntx_nrx){ 
		Vec.fill(params.max_ntx_nrx) {new ComplexSFix(w=params.fix_pt_wd, e = params.fix_pt_exp).asOutput } }

	// done flag
	val done = Bool().asOutput

	val probe = Vec.fill(params.max_ntx_nrx){ 
		Vec.fill(params.max_ntx_nrx) {new ComplexSFix(w=params.fix_pt_wd, e = params.fix_pt_exp).asOutput } }

	val probe_snr = SFix(width=params.fix_pt_wd, exp = REG_WD+1).asOutput
}


// Module to calculate the initial seed for the receive filter W
class InitializeWeights(implicit params: LMSParams) extends Module
{
	val io = new InitializeWeightsIO()

	// Hermitian of channel matrix 
	val channelMatrixHerm = Vec.fill(params.max_ntx_nrx){ 
		Vec.fill(params.max_ntx_nrx) {new ComplexSFix(w=params.fix_pt_wd, e = params.fix_pt_exp) } }

	for (i <- 0 until params.max_ntx_nrx) {
		for (j <- 0 until params.max_ntx_nrx) {
			channelMatrixHerm(i)(j) := conj(io.channelMatrix(j)(i))
		}
	}

	// stores product H*H_hermitian
	val product = Vec.fill(params.max_ntx_nrx){ 
		Vec.fill(params.max_ntx_nrx){ Reg(init = makeComplexSFix(w=params.fix_pt_wd, r=0, i=0)) } }

	// stores kernel H*H_hermitian + 1/SNR*eye(max_ntx_nrx)
	val kernel = Vec.fill(params.max_ntx_nrx){ 
		Vec.fill(params.max_ntx_nrx) {new ComplexSFix(w=params.fix_pt_wd, e = params.fix_pt_exp) } }

	val counter = Reg(init = UInt(0,6))
	val process_inputs = io.start && (~io.rst) && (counter < UInt(6))
	val process_kernel = io.start && (~io.rst) && (counter > UInt(5)) && (counter < UInt(19))
	val process_output = io.start && (~io.rst) && (counter > UInt(18))

	val done = process_output && (counter >= UInt(36))
	
	when ((process_inputs || process_kernel || process_output) && ~done) {
		counter := counter + UInt(1)
	}
	when (io.rst) {
		counter := UInt(0)
	}

	// registers to store 1/snr*eye(Nant)
	val snr_mat = Vec.fill(params.max_ntx_nrx){ 
		Vec.fill(params.max_ntx_nrx){ new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp) } }

	val snr_test = (io.snr).toSInt
	val one_sfix = SFix(width = params.fix_pt_wd, exp=params.fix_pt_exp)
	val snr_sfix = SFix(exp=REG_WD+1, width = params.fix_pt_wd)

	one_sfix.raw := SInt(1) << UInt(params.fix_pt_frac_bits)
	snr_sfix.raw := ((io.snr) << UInt(params.fix_pt_wd - REG_WD - 1)).toBits
	io.probe_snr := snr_sfix

	val snr_inv = new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp)
	snr_inv.real := fix_div(one_sfix, snr_sfix)
	snr_inv.imag.raw := Bits(0)

	for (i <- 0 until params.max_ntx_nrx) {
		for (j <- 0 until params.max_ntx_nrx) {
			if (i == j) {
				snr_mat(i)(j) := snr_inv
			} else {
				snr_mat(i)(j) := makeComplexSFix(w=params.fix_pt_wd, r=0, i=0)
			}
		}
	}

	for (i <- 0 until params.max_ntx_nrx) {
		for (j <- 0 until params.max_ntx_nrx) {
			kernel(i)(j) := complex_add(product(i)(j), snr_mat(i)(j))
		}
	}

	val inverse2 = Module(new Mat2Inverse())
	//val inverse3 = Module(new Mat3Inverse())  // temporarily removed to speed up compilation
	val inverse4 = Module(new Mat4Inverse())

	inverse4.io.rst := (counter < UInt(8))
	inverse2.io.rst := (counter < UInt(8))

	for (i <- 0 until 2) {
		for (j <- 0 until 2) {
			inverse2.io.matIn(i)(j) := kernel(i)(j)
		}
	}
/*	for (i <- 0 until 3) {
		for (j <- 0 until 3) {
			inverse3.io.matIn(i)(j) := kernel(i)(j)
		}
	}
*/
	for (i <- 0 until 4) {
		for (j <- 0 until 4) {
			inverse4.io.matIn(i)(j) := kernel(i)(j)
		}
	}

	// inverse of the kernel
	val inverse = Vec.fill(params.max_ntx_nrx){ 
		Vec.fill(params.max_ntx_nrx) {Reg(init = makeComplexSFix(w=params.fix_pt_wd, r=0, i=0)) } }

	when (io.Nant === UInt(2)) {
		for (i <- 0 until 2) {
			for (j <- 0 until 2) {
				inverse(i)(j) := inverse2.io.matOut(i)(j)
			}
		}
/*
	} .elsewhen (io.Nant === UInt(3)) {
		for (i <- 0 until 3) {
			for (j <- 0 until 3) {
				inverse(i)(j) := inverse3.io.matOut(i)(j)
			}
		}
*/
	} .elsewhen (io.Nant === UInt(4)) {
		for (i <- 0 until 4) {
			for (j <- 0 until 4) {
				inverse(i)(j) := inverse4.io.matOut(i)(j)
			}
		}
	}

//	when (process_kernel) {
//	}

	// inverse of the kernel
	val result = Vec.fill(params.max_ntx_nrx){ 
		Vec.fill(params.max_ntx_nrx) {Reg(init = makeComplexSFix(w=params.fix_pt_wd, r=0, i=0)) } }

	io.probe := result

	when (process_inputs) {
		io.toMatEngine.matrixIn := channelMatrixHerm
		when (counter === UInt(1)) {
			for (i <- 0 until params.max_ntx_nrx) {
				io.toMatEngine.vectorIn(i) := io.channelMatrix(i)(0)
			}
		} .elsewhen (counter === UInt(2)) {
			for (i <- 0 until params.max_ntx_nrx) {
				io.toMatEngine.vectorIn(i) := io.channelMatrix(i)(1)
				product(i)(0) := io.toMatEngine.result(i)
			}
		} .elsewhen (counter === UInt(3)) {
			for (i <- 0 until params.max_ntx_nrx) {
				io.toMatEngine.vectorIn(i) := io.channelMatrix(i)(2)
				product(i)(1) := io.toMatEngine.result(i)
			}
		} .elsewhen (counter === UInt(4)) {
			for (i <- 0 until params.max_ntx_nrx) {
				io.toMatEngine.vectorIn(i) := io.channelMatrix(i)(3)
				product(i)(2) := io.toMatEngine.result(i)
			}
		} .otherwise {
			for (i <- 0 until params.max_ntx_nrx) {
				io.toMatEngine.vectorIn(i) := io.channelMatrix(i)(3)
				product(i)(3) := io.toMatEngine.result(i)
			}
		}
	} .elsewhen (process_output && ~done) {
		io.toMatEngine.matrixIn := inverse
		when (counter === UInt(31)) {
			for (i <- 0 until params.max_ntx_nrx) {
				io.toMatEngine.vectorIn(i) := channelMatrixHerm(i)(0)
			}
		} .elsewhen (counter === UInt(32)) {
			for (i <- 0 until params.max_ntx_nrx) {
				io.toMatEngine.vectorIn(i) := channelMatrixHerm(i)(1)
				result(i)(0) := io.toMatEngine.result(i)
			}
		} .elsewhen (counter === UInt(33)) {
			for (i <- 0 until params.max_ntx_nrx) {
				io.toMatEngine.vectorIn(i) := channelMatrixHerm(i)(2)
				result(i)(1) := io.toMatEngine.result(i)
			}
		} .elsewhen (counter === UInt(34)) {
			for (i <- 0 until params.max_ntx_nrx) {
				io.toMatEngine.vectorIn(i) := channelMatrixHerm(i)(3)
				result(i)(2) := io.toMatEngine.result(i)
			}
		} .otherwise {
			for (i <- 0 until params.max_ntx_nrx) {
				io.toMatEngine.vectorIn(i) := channelMatrixHerm(i)(3)
				result(i)(3) := io.toMatEngine.result(i)
			}
		}
	} .otherwise {
		io.toMatEngine.matrixIn := Vec.fill(params.max_ntx_nrx){ 
			Vec.fill(params.max_ntx_nrx){ makeComplexSFix(w=params.fix_pt_wd, r=0, i=0) } }
		io.toMatEngine.vectorIn := Vec.fill(params.max_ntx_nrx){makeComplexSFix(w=params.fix_pt_wd, r=0, i=0)}
	}

	io.initialW := result
	io.done := done
		
	
}


