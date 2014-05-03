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
	val snr = UInt(width=10).asInput

	// MatrixEngine IO for multiplying matrices
	val toMatEngine = new MatrixEngineIO().flip()

	// output matrix with initial W seed
	val initialW = Vec.fill(params.max_ntx_nrx){ 
		Vec.fill(params.max_ntx_nrx) {new ComplexSFix(w=params.fix_pt_wd, e = params.fix_pt_exp).asOutput } }

	// done flag
	val done = Bool().asOutput

	val probe = Vec.fill(params.max_ntx_nrx){ 
		Vec.fill(params.max_ntx_nrx) {new ComplexSFix(w=params.fix_pt_wd, e = params.fix_pt_exp).asOutput } }
}


// Module to estimate the channel matrix H
class InitializeWeights(implicit params: LMSParams) extends Module
{
	val io = new InitializeWeightsIO()

	// Hermitian of channel matrix 
	val channelMatrixHerm = Vec.fill(params.max_ntx_nrx){ 
		Vec.fill(params.max_ntx_nrx) {new ComplexSFix(w=params.fix_pt_wd, e = params.fix_pt_exp) } }

	for (i <- 0 until params.max_ntx_nrx) {
		for (j <- 0 until params.max_ntx_nrx) {
			channelMatrixHerm(i)(j) := conj(io.channelMatrix(i)(j))
		}
	}

	// stores product H*H_hermitian
	val product = Vec.fill(params.max_ntx_nrx){ 
		Vec.fill(params.max_ntx_nrx){ Reg(init = makeComplexSFix(w=params.fix_pt_wd, r=0, i=0)) } }

	// stores kernel H*H_hermitian + 1/SNR*eye(max_ntx_nrx)
	val kernel = Vec.fill(params.max_ntx_nrx){ 
		Vec.fill(params.max_ntx_nrx) {new ComplexSFix(w=params.fix_pt_wd, e = params.fix_pt_exp) } }

	val counter = Reg(init = UInt(0,5))
	val process_inputs = io.start && (~io.rst) && (counter < UInt(6))
	val process_kernel = io.start && (~io.rst) && (counter > UInt(5)) && (counter < UInt(12))
	val process_output = io.start && (~io.rst) && (counter > UInt(11))

	val done = process_output && (counter === UInt(18))
	
	when ((process_inputs || process_kernel || process_output) && ~done) {
		counter := counter + UInt(1)
	}
	when (io.rst) {
		counter := UInt(0)
	}

	// registers to store 1/snr*eye(Nant)
	val snr_mat = Vec.fill(params.max_ntx_nrx){ 
		Vec.fill(params.max_ntx_nrx){ new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp) } }

	val snr_inv = new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp)
	snr_inv.real.raw := UInt(1) / io.snr
	snr_inv.imag.raw := Bits(0)

	io.probe := snr_mat

	for (i <- 0 until params.max_ntx_nrx) {
		snr_mat(i)(i) := snr_inv
	}

	for (i <- 0 until params.max_ntx_nrx) {
		for (j <- 0 until params.max_ntx_nrx) {
			kernel(i)(j) := complex_add(product(i)(j), snr_mat(i)(j))
		}
	}

	val inverse2 = Module(new Mat2Inverse())
	val inverse3 = Module(new Mat3Inverse())
	val inverse4 = Module(new Mat4Inverse())

	inverse4.io.rst := (counter < UInt(7))

	// inverse of the kernel
	val inverse = Vec.fill(params.max_ntx_nrx){ 
		Vec.fill(params.max_ntx_nrx) {Reg(init = makeComplexSFix(w=params.fix_pt_wd, r=0, i=0)) } }

	when (process_kernel) {
		when (io.Nant === UInt(2)) {
			for (i <- 0 until 2) {
				for (j <- 0 until 2) {
					inverse2.io.matIn(i)(j) := kernel(i)(j)
					inverse(i)(j) := inverse2.io.matOut(i)(j)
				}
			}
		} .elsewhen (io.Nant === UInt(1)) {
			for (i <- 0 until 3) {
				for (j <- 0 until 3) {
					inverse3.io.matIn(i)(j) := kernel(i)(j)
					inverse(i)(j) := inverse3.io.matOut(i)(j)
				}
			}
		} .otherwise {
			for (i <- 0 until 4) {
				for (j <- 0 until 4) {
					inverse4.io.matIn(i)(j) := kernel(i)(j)
					inverse(i)(j) := inverse4.io.matOut(i)(j)
				}
			}
		}
	} .otherwise {
		inverse2.io.matIn := Vec.fill(2){ Vec.fill(2){makeComplexSFix(w=params.fix_pt_wd, r=0, i=0) } }
		inverse3.io.matIn := Vec.fill(3){ Vec.fill(3){makeComplexSFix(w=params.fix_pt_wd, r=0, i=0) } }
		inverse4.io.matIn := Vec.fill(4){ Vec.fill(4){makeComplexSFix(w=params.fix_pt_wd, r=0, i=0) } }
	}

	// inverse of the kernel
	val result = Vec.fill(params.max_ntx_nrx){ 
		Vec.fill(params.max_ntx_nrx) {Reg(init = makeComplexSFix(w=params.fix_pt_wd, r=0, i=0)) } }

	when (process_inputs) {
		io.toMatEngine.matrixIn := channelMatrixHerm
		when (counter === UInt(1)) {
			io.toMatEngine.vectorIn := io.channelMatrix(0)
		} .elsewhen (counter === UInt(2)) {
			io.toMatEngine.vectorIn := io.channelMatrix(1)
			product(0) := io.toMatEngine.result
		} .elsewhen (counter === UInt(3)) {
			io.toMatEngine.vectorIn := io.channelMatrix(2)
			product(1) := io.toMatEngine.result
		} .elsewhen (counter === UInt(4)) {
			io.toMatEngine.vectorIn := io.channelMatrix(3)
			product(2) := io.toMatEngine.result
		} .otherwise {
			io.toMatEngine.vectorIn := io.channelMatrix(3)
			product(3) := io.toMatEngine.result
		}
	} .elsewhen (process_output && ~done) {
		io.toMatEngine.matrixIn := inverse
		when (counter === UInt(1)) {
			io.toMatEngine.vectorIn := io.channelMatrix(0)
		} .elsewhen (counter === UInt(2)) {
			io.toMatEngine.vectorIn := io.channelMatrix(1)
			result(0) := io.toMatEngine.result
		} .elsewhen (counter === UInt(3)) {
			io.toMatEngine.vectorIn := io.channelMatrix(2)
			result(1) := io.toMatEngine.result
		} .elsewhen (counter === UInt(4)) {
			io.toMatEngine.vectorIn := io.channelMatrix(3)
			result(2) := io.toMatEngine.result
		} .otherwise {
			io.toMatEngine.vectorIn := io.channelMatrix(3)
			result(3) := io.toMatEngine.result
		}
	} .otherwise {
		io.toMatEngine.matrixIn := Vec.fill(params.max_ntx_nrx){ 
			Vec.fill(params.max_ntx_nrx){ makeComplexSFix(w=params.fix_pt_wd, r=0, i=0) } }
		io.toMatEngine.vectorIn := Vec.fill(params.max_ntx_nrx){makeComplexSFix(w=params.fix_pt_wd, r=0, i=0)}
	}

	io.initialW := result
	io.done := done
		
	
}


