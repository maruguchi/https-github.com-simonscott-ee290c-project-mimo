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
	val trainSequence = Vec.fill(params.max_nrx){
                    Vec.fill(params.max_train_len){new ComplexSFix(10)}}

	// Nrx by trainLength matrix of input data
	val dataIn = Vec.fill(params.max_train_len){
                    Vec.fill(params.max_nrx){new ComplexSFix(10)}}

	// output with channel estimate matrix
	val channelOut = Vec.fill(params.max_ntx){
                    Vec.fill(params.max_nrx){new ComplexSFix(16)}}

	// sends dataIn matrix to matrix engine
	val matToEngine = Vec.fill(params.max_train_len){
                    Vec.fill(params.max_nrx){new ComplexSFix(10)}}

	// sends one column of trainSequence (or transpose) to matrix engine for processing with matToEngine
	val vecToEngine = Vec.fill(params.max_train_len){new ComplexSFix(10)}

	// processed vector from matrix engine = matToEngine * vecToEngine
	val vecFromEngine = Vec.fill(params.max_nrx){new ComplexSFix(12)}
}


// Module to estimate the channel matrix H
class ChannelEstimator(implicit params: LMSParams) extends Module
{
    val io = new ChannelEstimatorIO()

    // always sends dataIn as matrix to engine
    io.matToEngine := io.dataIn

    // sends each column of trainSequence to engine and fills channel estimate with each corresponding received column
    for (t <-0 until params.max_nrx) {
	io.vecToEngine := io.trainSequence(t)
	io.channelOut(t) := io.vecFromEngine
    }
}


