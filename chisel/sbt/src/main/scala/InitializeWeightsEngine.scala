package Work

import Chisel._
import Node._
import scala.collection.mutable.HashMap
import scala.math._
import LMSConstants._
import ComplexMathFunctions._
import FixedPoint._

// provides a class to test together the channel initializer and the matrix engine

class InitializeWeightsEngineIO(implicit params: LMSParams) extends Bundle()
{
	// input flag; will start processing when start goes high
	val start = Bool().asInput

	// resets initializer into initial state to receive new matrix
	val rst = Bool().asInput

	// output with channel estimate matrix
	val channelMatrix = Vec.fill(params.max_ntx_nrx){
                    Vec.fill(params.max_ntx_nrx){new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp).asInput}}
	
	// Number of Tx/Rx antennas
	val Nant = UInt(width = REG_WD).asInput

	// SNR (linear)
	val snr = UInt(width=REG_WD).asInput

	// output matrix with initial W seed
	val initialW = Vec.fill(params.max_ntx_nrx){ 
		Vec.fill(params.max_ntx_nrx) {new ComplexSFix(w=params.fix_pt_wd, e = params.fix_pt_exp).asOutput } }

	// output flag; goes high when done processing
	val done = Bool().asOutput

	val probe = Vec.fill(params.max_ntx_nrx){ 
		Vec.fill(params.max_ntx_nrx) {new ComplexSFix(w=params.fix_pt_wd, e = params.fix_pt_exp).asOutput } }

	val probe_snr = SFix(width=params.fix_pt_wd, exp = REG_WD+1).asOutput
}


class InitializeWeightsEngine(implicit params: LMSParams) extends Module
{
    val io = new InitializeWeightsEngineIO()

    val initializer = Module(new InitializeWeights())
    val engine = Module(new MatrixEngine())

    io.probe := initializer.io.probe
    io.probe_snr := initializer.io.probe_snr

    initializer.io.channelMatrix := io.channelMatrix
    initializer.io.start := io.start
    initializer.io.rst := io.rst
    initializer.io.snr := io.snr
    initializer.io.Nant := io.Nant
    io.initialW := initializer.io.initialW
    io.done := initializer.io.done

    engine.io <> initializer.io.toMatEngine  

}

class InitializeWeightsEngineTests(c: InitializeWeightsEngine, params: LMSParams) extends Tester(c)
{
val matIn_r = Array( Array(1.3,-2.1,1.2,1), Array(0.3,-0.5,-0.4,-1), Array(0.2,0.5,-0.9,1), Array(0.1,0.95,-0.3,-1))
val matIn_i = Array( Array(1.1,0.7,2.1,1), Array(-1.1,1.1,0.3,1), Array(-0.3,0.5,-0.7,1), Array(-0.3,0.5,-0.7,1))

val snr = 20

// Apply inputs
for (t <- 0 until 1)
{

	poke(c.io.rst, 0)
	poke(c.io.start, 1)
	poke(c.io.Nant, 2)
	poke(c.io.snr, snr)

	for (i <- 0 until 4) {
		for (j <- 0 until 4) {
			poke(c.io.channelMatrix(i)(j).real.raw, conv_double_to_fp(matIn_r(i)(j), params.fix_pt_frac_bits, params.fix_pt_wd))
			poke(c.io.channelMatrix(i)(j).imag.raw, conv_double_to_fp(matIn_i(i)(j), params.fix_pt_frac_bits, params.fix_pt_wd))
		}
	}

	step(26)
	peek(c.initializer.inverse4.io.rst)
	for (i <- 0 until 4) {
		for (j <- 0 until 4) {
			println( conv_fp_to_double(peek(c.io.probe(i)(j).real.raw), params.fix_pt_frac_bits, params.fix_pt_wd) )
			println( conv_fp_to_double(peek(c.io.probe(i)(j).imag.raw), params.fix_pt_frac_bits, params.fix_pt_wd) )

//initializer.inverse4.io.matOut
//io.probe
		}
	}
	peek(c.io.done)
	step(1)
	peek(c.io.done)
	step(1)
	peek(c.io.done)
	step(1)
	peek(c.io.done)
	step(1)
	peek(c.io.done)
	step(1)
	peek(c.io.done)
	step(1)

}
}
