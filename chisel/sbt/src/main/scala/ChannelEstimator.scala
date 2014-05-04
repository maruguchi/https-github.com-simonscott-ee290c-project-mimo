package Work

import Chisel._
import Node._
import scala.collection.mutable.HashMap
import scala.math._
import LMSConstants._
import ComplexMathFunctions._
import FixedPoint._

// Notes from discussion on April 10:
// H should be signed integers at the output (multiply weights by 4)
// These integers will probably be 16 bits wide, as samples are 10 bit
// Also, rewrite the MATLAB code to perform this as mat-vec multiply.
// The mat-vec multiply can be done using the matrix engine

class ChannelEstimatorIO(implicit params: LMSParams) extends Bundle()
{
	// input flag; will start processing when start goes high
	val start = Bool().asInput

	// resets estimator into initial state to receive new matrix
	val rst = Bool().asInput

	// output flag; goes high when done processing
	val done = Bool().asOutput

	// sequence of training symbols
	val trainSequence = Vec.fill(params.max_train_len){new ComplexSFix(w=params.samp_wd, e=params.samp_exp).asInput}

	// address to training symbol memory
	val trainAddress = UInt(width=2).asOutput

	// stream of input data
	val dataIn = Decoupled( Vec.fill(params.max_ntx_nrx){new ComplexSFix(w=params.samp_wd, e=params.samp_exp)} ).flip()

	// output with channel estimate matrix
	val channelOut = Vec.fill(params.max_ntx){
                    Vec.fill(params.max_nrx){new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp).asOutput}}

	// interface to the matrix engine
	val toMatEngine = new MatrixEngineIO().flip()
	
	// Number of Tx/Rx antennas
	val Nant = UInt(width = REG_WD).asInput
}


// Module to estimate the channel matrix H
class ChannelEstimator(implicit params: LMSParams) extends Module
{
    val io = new ChannelEstimatorIO()

    val dataMatrix = Vec.fill(params.max_train_len){ 
	Vec.fill(params.max_ntx_nrx){ Reg(new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp)) } }

    val trainMatrix = Vec.fill(params.max_train_len){ 
	Vec.fill(params.max_ntx_nrx){ Reg(new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp)) } }

    val input_counter = Reg(init = UInt(0,3))
    val process_inputs = io.start && io.dataIn.valid && (input_counter < UInt(params.max_ntx_nrx)) && (~io.rst)
    
    io.trainAddress := input_counter

    when (process_inputs) {
	input_counter := input_counter + UInt(1)
    }
    when (io.rst) {
	input_counter := UInt(0)
    }
    io.dataIn.ready := process_inputs

    // Bit shift inputs to fix_pt widths and exponents
    for(i <- 0 until params.max_ntx_nrx)
    {
	when (process_inputs) {
		dataMatrix(input_counter)(i).real.raw := io.dataIn.bits(i).real.raw << UInt(params.fix_pt_frac_bits - params.samp_frac_bits)
		dataMatrix(input_counter)(i).imag.raw := io.dataIn.bits(i).imag.raw << UInt(params.fix_pt_frac_bits - params.samp_frac_bits)
		trainMatrix(input_counter)(i).real.raw := io.trainSequence(i).real.raw << UInt(params.fix_pt_frac_bits - params.samp_frac_bits)
		trainMatrix(input_counter)(i).imag.raw := io.trainSequence(i).imag.raw << UInt(params.fix_pt_frac_bits - params.samp_frac_bits)
	}
    }

    // start output processing once all the inputs are received
    val process_outputs = (input_counter === UInt(params.max_ntx_nrx))
    val output_counter = Reg(init = UInt(0,3))

    when (io.rst) {
	output_counter := UInt(0)
    }

    val done = process_outputs && (output_counter === UInt(params.max_ntx_nrx + 1))

    // always sends dataIn as matrix to engine
    io.toMatEngine.matrixIn := dataMatrix

    val result = Vec.fill(params.max_ntx_nrx){ 
	Vec.fill(params.max_ntx_nrx){ Reg(init = makeComplexSFix(w=params.fix_pt_wd, r=0, i=0)) } }

    // send and receive appropriate column to matrix engine
    when (process_outputs && ~done) {
	    output_counter := output_counter + UInt(1)
	    when (output_counter === UInt(0)) {
		io.toMatEngine.vectorIn := trainMatrix(0)
	    } .elsewhen (output_counter === UInt(1)) {
		io.toMatEngine.vectorIn := trainMatrix(1)
		for (i <- 0 until params.max_ntx_nrx) {
			result(i)(0).real.raw := io.toMatEngine.result(i).real.raw >> (io.Nant >> UInt(1))
			result(i)(0).imag.raw := io.toMatEngine.result(i).imag.raw >> (io.Nant >> UInt(1))
		}
	    } .elsewhen (output_counter === UInt(2)) {
		io.toMatEngine.vectorIn := trainMatrix(2)
		for (i <- 0 until params.max_ntx_nrx) {
			result(i)(1).real.raw := io.toMatEngine.result(i).real.raw >> (io.Nant >> UInt(1))
			result(i)(1).imag.raw := io.toMatEngine.result(i).imag.raw >> (io.Nant >> UInt(1))
		}
	    } .elsewhen (output_counter === UInt(3)) {
		io.toMatEngine.vectorIn := trainMatrix(3)
		for (i <- 0 until params.max_ntx_nrx) {
			result(i)(2).real.raw := io.toMatEngine.result(i).real.raw >> (io.Nant >> UInt(1))
			result(i)(2).imag.raw := io.toMatEngine.result(i).imag.raw >> (io.Nant >> UInt(1))
		}
	    } .otherwise {
		io.toMatEngine.vectorIn := trainMatrix(3)
		for (i <- 0 until params.max_ntx_nrx) {
			result(i)(3).real.raw := io.toMatEngine.result(i).real.raw >> (io.Nant >> UInt(1))
			result(i)(3).imag.raw := io.toMatEngine.result(i).imag.raw >> (io.Nant >> UInt(1))
		}
	    }
    } .otherwise {
		io.toMatEngine.vectorIn := Vec.fill(params.max_ntx_nrx){makeComplexSFix(w=params.fix_pt_wd, r=0, i=0)}
    }

    io.channelOut := result
    io.done := done

}

