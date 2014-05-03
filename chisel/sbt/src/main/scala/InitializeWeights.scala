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
	val snr = SFix(width = REG_WD,exp = params.fix_pt_exp).asInput

	// MatrixEngine IO for multiplying matrices
	val toMatEngine = new MatrixEngineIO().flip()

	// output matrix with initial W seed
	val initialW = Vec.fill(params.max_ntx_nrx){ 
		Vec.fill(params.max_ntx_nrx) {new ComplexSFix(w=params.fix_pt_wd, e = params.fix_pt_exp).asOutput } }

	// done flag
	val done = Bool().asOutput
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
		Vec.fill(params.max_ntx_nrx) {new ComplexSFix(w=params.fix_pt_wd, e = params.fix_pt_exp) } }

	// stores kernel H*H_hermitian + SNR*eye(max_ntx_nrx)
	val kernel = Vec.fill(params.max_ntx_nrx){ 
		Vec.fill(params.max_ntx_nrx) {new ComplexSFix(w=params.fix_pt_wd, e = params.fix_pt_exp) } }

	val snr_fix = new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp)
	snr_fix.real := io.snr
	snr_fix.imag := makeComplexSFix(w=params.fix_pt_wd, r=0, i=0)

	val input_counter = Reg(init = UInt(0,4))
	val process_inputs = io.start && (~io.rst) && (input_counter < UInt(6))
	val process_kernel = io.start && (~io.rst) && (input_counter > UInt(5)) && (input_counter < UInt(12))
	val process_output = io.start && (~io.rst) && (input_counter < UInt(6))
	
	when (process_inputs) {
		input_counter := input_counter + UInt(1)
	}
	when (io.rst) {
		input_counter := UInt(0)
	}

	when (process_inputs) {
		io.toMatEngine.matrixIn := channelMatrixHerm
		when (input_counter === UInt(1)) {
			io.toMatEngine.vectorIn := io.channelMatrix(0)
		} .elsewhen (input_counter === UInt(2)) {
			io.toMatEngine.vectorIn := io.channelMatrix(1)
			product(0) := io.toMatEngine.result
		} .elsewhen (input_counter === UInt(3)) {
			io.toMatEngine.vectorIn := io.channelMatrix(2)
			product(1) := io.toMatEngine.result
		} .elsewhen (input_counter === UInt(4)) {
			io.toMatEngine.vectorIn := io.channelMatrix(3)
			product(2) := io.toMatEngine.result
		} .otherwise {
			io.toMatEngine.vectorIn := io.channelMatrix(3)
			product(3) := io.toMatEngine.result
		}
	} .otherwise {
		io.toMatEngine.vectorIn := Vec.fill(params.max_ntx_nrx){makeComplexSFix(w=params.fix_pt_wd, r=0, i=0)}
	}

	val inverse2 = Module(new Mat2Inverse())
	val inverse3 = Module(new Mat3Inverse())
	val inverse4 = Module(new Mat4Inverse())

	inverse4.io.rst := (input_counter < UInt(7))

	// inverse of the kernel
	val inverse = Vec.fill(params.max_ntx_nrx){ 
		Vec.fill(params.max_ntx_nrx) {Reg(init = makeComplexSFix(w=params.fix_pt_wd, r=0, i=0)) } }

	when (process_kernel) {
		for (i <- 0 until params.max_ntx_nrx) {
			for (j <- 0 until params.max_ntx_nrx) {
				when (i == j) {
					kernel(i)(j) := product(i)(j) + snr_sfix
				} .otherwise {
					kernel(i)(j) := product(i)(j)
				}
			}
		}

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
	}
		
	
}


