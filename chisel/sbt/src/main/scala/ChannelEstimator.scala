package Work

import Chisel._
import Node._
import scala.collection.mutable.HashMap
import scala.math._
import LMSConstants._

// Notes from discussion on April 10:
// H should be signed integers at the output (multiply weights by 4)
// These integers will probably be 16 bits wide, as samples are 10 bit
// Also, rewrite the MATLAB code to perform this as mat-vec multiply.
// The mat-vec multiply can be done using the matrix engine

class ChannelEstimatorIO(implicit params: LMSParams) extends Bundle()
{
	// trainLength by Nrx matrix of training sequence (orthogonal)
	// might have to resize and add a transpose function depending on source data format
	val trainSequence = Vec.fill(params.max_ntx_nrx){
                    Vec.fill(params.max_train_len){new ComplexSFix(10).asInput}}

	// Nrx by trainLength matrix of input data
	val dataIn = Vec.fill(params.max_train_len){
                    Vec.fill(params.max_ntx_nrx){new ComplexSFix(10).asInput}}

	// output with channel estimate matrix
	val channelOut = Vec.fill(params.max_ntx){
                    Vec.fill(params.max_nrx){new ComplexSFix(16).asOutput}}

	// sends dataIn matrix to matrix engine
	val matToEngine = Vec.fill(params.max_train_len){
                    Vec.fill(params.max_nrx){new ComplexSFix(10).asOutput}}

	// sends one column of trainSequence (or transpose) to matrix engine for processing with matToEngine
	val vecToEngine = Vec.fill(params.max_train_len){new ComplexSFix(10).asOutput}

	// processed vector from matrix engine = matToEngine * vecToEngine
	val vecFromEngine = Vec.fill(params.max_ntx_nrx){new ComplexSFix(12).asInput}
}


// Module to estimate the channel matrix H
class ChannelEstimator(implicit params: LMSParams) extends Module
{
    val io = new ChannelEstimatorIO()

    // always sends dataIn as matrix to engine
    io.matToEngine := io.dataIn

    val counter = Reg(init = UInt(0,3))
    counter := counter + UInt(1);
    val register0 = Vec.fill(params.max_ntx_nrx){ Reg(new ComplexSFix(12)) }
    val register1 = Vec.fill(params.max_ntx_nrx){ Reg(new ComplexSFix(12)) }
    val register2 = Vec.fill(params.max_ntx_nrx){ Reg(new ComplexSFix(12)) }
    val register3 = Vec.fill(params.max_ntx_nrx){ Reg(new ComplexSFix(12)) }

    when (counter === UInt(0)) {
	io.vecToEngine := io.trainSequence(0)
    } .elsewhen (counter === UInt(1)) {
	io.vecToEngine := io.trainSequence(1)
	register0 := io.vecFromEngine
    } .elsewhen (counter === UInt(2)) {
	io.vecToEngine := io.trainSequence(2)
	register1 := io.vecFromEngine
    } .elsewhen (counter === UInt(3)) {
	io.vecToEngine := io.trainSequence(3)
	register2 := io.vecFromEngine
    } .otherwise {
	io.vecToEngine := io.trainSequence(3)
	register3 := io.vecFromEngine
    }

    io.channelOut(0) := register0
    io.channelOut(1) := register1
    io.channelOut(2) := register2
    io.channelOut(3) := register3

    // sends each column of trainSequence to engine and fills channel estimate with each corresponding received column
//    for (i <-0 until params.max_ntx_nrx) {
//	io.vecToEngine(i) := io.trainSequence(i)(counter)
//	io.channelOut(i)(counter) := io.vecFromEngine(i)
//    }
}

