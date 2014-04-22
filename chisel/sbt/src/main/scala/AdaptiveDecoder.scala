package Work

import Chisel._
import Node._
import scala.collection.mutable.HashMap
import scala.math._
import LMSConstants._
import ComplexMathFunctions._
import FixedPoint._

/*
 * Important TODOs to get basic decoding working:
 * 1.) Handle case when W is being computed, and we mustn't start pulling samples from the queue
 * 2.) Only write valid bit on decodedData queue when we have valid W and valid samples
 * 4.) Write test bench: check that decodes correctly, don't worry about adapting
 */

// I/O interface for the adaptive decoder
// Note: decodedData contains integers representing the symbols, e.g. 0 for (0 + 0j), 1 for (0 + 1j), 2 for (1 + 0j) 
class AdaptiveDecoderIO(implicit params: LMSParams) extends Bundle()
{
    val wSeed = Vec.fill(params.max_ntx_nrx){
                    Vec.fill(params.max_ntx_nrx){new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp).asInput}}

    val samples = Decoupled( Vec.fill(params.max_ntx_nrx){new ComplexSInt(w = params.samp_wd)} ).flip()

    val decodedData = Decoupled( Vec.fill(params.max_ntx_nrx){UInt(width = params.symbol_wd)} )

    val toMatEngine = new MatrixEngineIO().flip()

    val resetW = Bool().asInput
}


// Module to estimate the channel matrix H
class AdaptiveDecoder(implicit params: LMSParams) extends Module
{
    // Create the I/O
    val io = new AdaptiveDecoderIO()

    // Convert samples to fixed point
    // Samples are ints: treat top 4 bits as integer, rest as fractional.
    val samples_fp = Vec.fill(params.max_ntx_nrx){new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp)}
    for(i <- 0 until params.max_ntx_nrx)
    {
        samples_fp(i).real.raw := io.samples.bits(i).real << UInt(params.fix_pt_frac_bits - params.samp_wd + params.samp_int_bits)
        samples_fp(i).imag.raw := io.samples.bits(i).imag << UInt(params.fix_pt_frac_bits - params.samp_wd + params.samp_int_bits)
    }

    // ****** Hardware to perform the reading/writing of matrix W ******

    // Create wires
    val writeWUpdate = (io.samples.valid & io.decodedData.ready)
    val nextW = Vec.fill(params.max_ntx_nrx){ Vec.fill(params.max_ntx_nrx){
                new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp) } }

    // Create register array to store W
    val w = Vec.fill(params.max_ntx_nrx){ Vec.fill(params.max_ntx_nrx){
                Reg(new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp)) } }

    for(i <- 0 until params.max_ntx_nrx)
    {
        for(j <- 0 until params.max_ntx_nrx)
        {
            // Initialize register array with W seed
            when(io.resetW) {
                w(i)(j) := io.wSeed(i)(j)
            }

            // Otherwise update W with computed updates
            .elsewhen(writeWUpdate) {
                w(i)(j) := nextW(i)(j)
            }
        }
    }

    // ****** Hardware to compute the transmitted signal ******

    // Compute Wx
    io.toMatEngine.matrixIn := w
    io.toMatEngine.vectorIn := samples_fp
    val Wx = io.toMatEngine.result

    // Use slicer to decode
    // Assume QAM modulation for now
    val symbols = Vec.fill(params.max_ntx_nrx){UInt(width = params.symbol_wd)}
    for(i <- 0 until params.max_ntx_nrx)
    {
        when(Wx(i).imag.raw >= SInt(0) && Wx(i).real.raw >= SInt(0)) {
            symbols(i) := UInt(0)
        }
        .elsewhen(Wx(i).imag.raw >= SInt(0) && Wx(i).real.raw < SInt(0)) {
            symbols(i) := UInt(1)
        }
        .elsewhen(Wx(i).imag.raw < SInt(0) && Wx(i).real.raw >= SInt(0)) {
            symbols(i) := UInt(3)
        }
        .otherwise {
            symbols(i) := UInt(2)
        }

        io.decodedData.bits(i) := symbols(i)
    }

    // ****** Hardware to adapt the W matrix ******

    // Lots of TODO
    // Compute error, using a lookup table to compare computed symbol to predicted symbol
}


// Tester for testing the adaptive decoder
class AdaptiveDecoderTests(c: AdaptiveDecoder, params: LMSParams) extends Tester(c)
{
    // Create a matrix engine for testing purposes
    val matrixEngine = Module(new MatrixEngine()(params))
    c.io.toMatEngine <> matrixEngine.io
    
}


