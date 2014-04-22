package Work

import Chisel._
import Node._
import scala.collection.mutable.HashMap
import scala.math._
import LMSConstants._
import ComplexMathFunctions._
import FixedPoint._

// I/O interface for the adaptive decoder
// Note: decodedData contains integers representing the symbols, e.g. 0 for (0 + 0j), 1 for (0 + 1j), 2 for (1 + 0j) 
class AdaptiveDecoderIO(implicit params: LMSParams) extends Bundle()
{
    val wSeed = Vec.fill(params.max_ntx_nrx){
                    Vec.fill(params.max_ntx_nrx){new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp).asInput}}

    val samples = Decoupled( Vec.fill(params.max_ntx_nrx){new ComplexSInt(w = params.samp_wd)} ).flip()

    val decodedData = Decoupled( Vec.fill(params.max_ntx_nrx){UInt(width = params.symbol_wd)} )

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
        //samples_fp(i).real.raw = io.samples(i).real << (params.fix_pt_exp - params.samp_wd + params.samp_int_bits)
        //samples_fp(i).imag.raw = io.samples(i).imag << (params.fix_pt_exp - params.samp_wd + params.samp_int_bits)
    }

    // ****** Hardware to perform the reading/writing of matrix W ******

    // Create register array to store W
    val w = Vec.fill(params.max_ntx_nrx){ Vec.fill(params.max_ntx_nrx){
                Reg(new ComplexSFix(w=params.fix_pt_wd, e=params.fix_pt_exp)) } }

    // Initialize register array with W seed
    when(io.resetW)
    {
        for(i <- 0 until params.max_ntx_nrx) {
            for(j <- 0 until params.max_ntx_nrx) {
                w(i)(j) := io.wSeed(i)(j)
            }
        }
    }

    // ****** Hardware to compute the transmitted signal ******

    // Compute Wx

    // Use slicer to decode

    // ****** Hardware to adapt the W matrix ******

    // Compute error
}


// Tester for testing the adaptive decoder
class AdaptiveDecoderTests(c: AdaptiveDecoder, params: LMSParams) extends Tester(c)
{
}


