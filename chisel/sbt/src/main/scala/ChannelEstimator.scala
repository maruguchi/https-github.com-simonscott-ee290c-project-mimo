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

	// output flag; goes high when done processing
	val done = Bool().asOutput

	// sequence of training symbols
	val trainSequence = Vec.fill(params.max_train_len){new ComplexSInt(w = params.samp_wd).asInput}

	// address to training symbol memory
	val trainAddress = UInt(width=2).asOutput

	// stream of input data
	val dataIn = Decoupled( Vec.fill(params.max_ntx_nrx){new ComplexSInt(w = params.samp_wd)} ).flip()

	// output with channel estimate matrix
	val channelOut = Vec.fill(params.max_ntx){
                    Vec.fill(params.max_nrx){new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp).asOutput}}

	// interface to the matrix engine
	val toMatEngine = new MatrixEngineIO().flip()

    	val probe = Vec.fill(params.max_train_len){ 
		Vec.fill(params.max_ntx_nrx){ new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp).asOutput } }
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
    val process_inputs = io.start && io.dataIn.valid && (input_counter < UInt(params.max_ntx_nrx))
    
    io.trainAddress := input_counter
    io.probe := trainMatrix

    when (process_inputs) {
	input_counter := input_counter + UInt(1)
    }

    // Bit shift inputs to fix_pt widths and exponents
    for(i <- 0 until params.max_ntx_nrx)
    {
	when (process_inputs) {
        	dataMatrix(i)(input_counter).real.raw := io.dataIn.bits(i).real << UInt(params.fix_pt_frac_bits - params.samp_wd + params.samp_exp)
        	dataMatrix(i)(input_counter).imag.raw := io.dataIn.bits(i).imag << UInt(params.fix_pt_frac_bits - params.samp_wd + params.samp_exp)
		trainMatrix(input_counter)(i).real.raw := io.trainSequence(i).real << UInt(params.fix_pt_frac_bits - params.samp_wd + params.samp_exp)
        	trainMatrix(input_counter)(i).imag.raw := io.trainSequence(i).imag << UInt(params.fix_pt_frac_bits - params.samp_wd + params.samp_exp)
	}
    }

    // start output processing once all the inputs are received
    val process_outputs = (input_counter === UInt(params.max_ntx_nrx))
    val output_counter = Reg(init = UInt(0,3))

    val done = process_outputs && (output_counter === UInt(params.max_ntx_nrx + 1))

    // always sends dataIn as matrix to engine
    io.toMatEngine.matrixIn := dataMatrix

    val register0 = Vec.fill(params.max_ntx_nrx){ Reg(init = makeComplexSFix(w=params.fix_pt_wd, r=0, i=0)) }
    val register1 = Vec.fill(params.max_ntx_nrx){ Reg(init = makeComplexSFix(w=params.fix_pt_wd, r=0, i=0)) }
    val register2 = Vec.fill(params.max_ntx_nrx){ Reg(init = makeComplexSFix(w=params.fix_pt_wd, r=0, i=0)) }
    val register3 = Vec.fill(params.max_ntx_nrx){ Reg(init = makeComplexSFix(w=params.fix_pt_wd, r=0, i=0)) }

    // send and receive appropriate column to matrix engine
    when (process_outputs && ~done) {
	    output_counter := output_counter + UInt(1)
	    when (output_counter === UInt(0)) {
		io.toMatEngine.vectorIn := trainMatrix(0)
	    } .elsewhen (output_counter === UInt(1)) {
		io.toMatEngine.vectorIn := trainMatrix(1)
		register0 := io.toMatEngine.result
	    } .elsewhen (output_counter === UInt(2)) {
		io.toMatEngine.vectorIn := trainMatrix(2)
		register1 := io.toMatEngine.result
	    } .elsewhen (output_counter === UInt(3)) {
		io.toMatEngine.vectorIn := trainMatrix(3)
		register2 := io.toMatEngine.result
	    } .otherwise {
		io.toMatEngine.vectorIn := trainMatrix(3)
		register3 := io.toMatEngine.result
	    }
    } .otherwise {
		io.toMatEngine.vectorIn := Vec.fill(params.max_ntx_nrx){makeComplexSFix(w=params.fix_pt_wd, r=0, i=0)}
    }

    io.channelOut(0) := register0
    io.channelOut(1) := register1
    io.channelOut(2) := register2
    io.channelOut(3) := register3
    io.done := done

}

