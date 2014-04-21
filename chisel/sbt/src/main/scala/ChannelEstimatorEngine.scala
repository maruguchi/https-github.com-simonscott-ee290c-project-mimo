package Work

import Chisel._
import Node._
import scala.collection.mutable.HashMap
import scala.math._
import LMSConstants._

// provides a class to test together the channel estimator and the matrix engine

class ChannelEstimatorEngineIO(implicit params: LMSParams) extends Bundle()
{
	// trainLength by Nrx matrix of training sequence (orthogonal)
	// might have to resize and add a transpose function depending on source data format
	val trainSequence = Vec.fill(params.max_ntx_nrx){
                    Vec.fill(params.max_train_len){new ComplexSFix(10).asInput}}

	// Nrx by trainLength matrix of input data
	val dataIn = Vec.fill(params.max_train_len){
                    Vec.fill(params.max_ntx_nrx){new ComplexSFix(10).asInput}}

	// output with channel estimate matrix
	val channelOut = Vec.fill(params.max_ntx_nrx){
                    Vec.fill(params.max_ntx_nrx){new ComplexSFix(16).asOutput}}
}


class ChannelEstimatorEngine(implicit params: LMSParams) extends Module
{
    val io = new ChannelEstimatorEngineIO()

    val estimator = Module(new ChannelEstimator())
    val engine = Module(new MatrixEngine())

    estimator.io.trainSequence := io.trainSequence
    estimator.io.dataIn := io.dataIn
    io.channelOut := estimator.io.channelOut

    engine.io.matrixIn := estimator.io.matToEngine
    engine.io.vectorIn := estimator.io.vecToEngine
    estimator.io.vecFromEngine := engine.io.result    

}

// Tester for testing the matrix engine
class ChannelEstimatorEngineTests(c: ChannelEstimatorEngine, params: LMSParams) extends Tester(c)
{
    val dataIn_r = Array( Array(1,2,3,4), Array(11,12,13,14), Array(21,22,23,24), Array(31,32,33,34) )
    val dataIn_i = Array( Array(0,0,0,0), Array(0,0,0,0), Array(0,0,0,0), Array(0,0,0,0) )
    val trainSeq_r = Array( Array(1,1,0,0), Array(0,1,0,0), Array(0,0,1,0), Array(0,0,0,1) )
    val trainSeq_i = Array( Array(0,0,0,0), Array(0,0,0,0), Array(0,0,0,0), Array(0,0,0,0) )
    val channelOut_r = Array( Array(3,23,43,63), Array(2,12,22,32), Array(3,13,23,33), Array(4,14,24,34) )
    val channelOut_i = Array( Array(0,0,0,0), Array(0,0,0,0), Array(0,0,0,0), Array(0,0,0,0) )

    for (t <- 0 until 1)
    {
        // Apply inputs
        for(i <- 0 until params.max_ntx_nrx) {
            for(j <- 0 until params.max_ntx_nrx) {
                poke(c.io.dataIn(i)(j).real.raw, dataIn_r(i)(j))
                poke(c.io.dataIn(i)(j).imag.raw, dataIn_i(i)(j))
            	poke(c.io.trainSequence(i)(j).real.raw, trainSeq_r(i)(j))
            	poke(c.io.trainSequence(i)(j).imag.raw, trainSeq_i(i)(j))
            }
        }

        // Clock the module
        step(1)
        step(1)
	step(1)
	step(1)
	step(1)
	step(1)

        // Check the output
        for(i <- 0 until params.max_ntx_nrx) {
		for(j <- 0 until params.max_ntx_nrx) {
            		expect(c.io.channelOut(i)(j).real.raw, channelOut_r(i)(j))
            		expect(c.io.channelOut(i)(j).imag.raw, channelOut_i(i)(j))
		}
        }
    }
}

