package Work

import Chisel._
import Node._
import scala.collection.mutable.HashMap
import scala.math._
import LMSConstants._
import ComplexMathFunctions._
import FixedPoint._


class MatrixArbiterIO(implicit params: LMSParams) extends Bundle()
{
    val toMatrixEngine = new MatrixEngineIO().flip()
    val toAdaptiveDecoder = new MatrixEngineIO()
    val toChannelEstimator = new MatrixEngineIO()
    val toInitializeWeights = new MatrixEngineIO()

    val reqAdaptiveDecoder = Bool().asInput
    val reqChannelEstimator = Bool().asInput
    val reqInitializeWeights = Bool().asInput

    override def clone: this.type = { new MatrixArbiterIO().asInstanceOf[this.type]; }
}


// Module to arbitrate between different modules requesting the Matrix Engine
class MatrixArbiter(implicit params: LMSParams) extends Module
{
    val io = new MatrixArbiterIO()

    // Priority order is:
    // 1.) ChannelEstimator
    // 2.) Initialize Weights
    // 3.) AdaptiveDecoder
    
    when(io.reqChannelEstimator) {
        io.toMatrixEngine.matrixIn := io.toChannelEstimator.matrixIn
        io.toMatrixEngine.vectorIn := io.toChannelEstimator.vectorIn

    }
    .elsewhen(io.reqInitializeWeights) {
        io.toMatrixEngine.matrixIn := io.toInitializeWeights.matrixIn
        io.toMatrixEngine.vectorIn := io.toInitializeWeights.vectorIn
    }
    .elsewhen(io.reqAdaptiveDecoder) {
        io.toMatrixEngine.matrixIn := io.toAdaptiveDecoder.matrixIn
        io.toMatrixEngine.vectorIn := io.toAdaptiveDecoder.vectorIn
    }
    .otherwise {
        io.toMatrixEngine.matrixIn := io.toAdaptiveDecoder.matrixIn
        io.toMatrixEngine.vectorIn := io.toAdaptiveDecoder.vectorIn
    }
    
    io.toChannelEstimator.result := Mux(io.reqChannelEstimator, io.toMatrixEngine.result, Bits(0, width=params.max_ntx_nrx*2*params.fix_pt_wd))
    io.toInitializeWeights.result := Mux(io.reqInitializeWeights, io.toMatrixEngine.result, Bits(0, width=params.max_ntx_nrx*2*params.fix_pt_wd))
    io.toAdaptiveDecoder.result := Mux(io.reqAdaptiveDecoder, io.toMatrixEngine.result, Bits(0, width=params.max_ntx_nrx*2*params.fix_pt_wd))
}


// Tester for testing the arbiter
class MatrixArbiterTests(c: MatrixArbiter, params: LMSParams) extends Tester(c)
{
    // Test vectors
    // Transmitted sequence is: [1 + 1j, -1 + 1j, -1 -1j, +1 - 1j]
    val ce_matrixIn = Array.fill(params.max_ntx_nrx, params.max_ntx_nrx)(1)
    val ce_vectorIn = Array.fill(params.max_ntx_nrx)(10)
    val iw_matrixIn = Array.fill(params.max_ntx_nrx, params.max_ntx_nrx)(2)
    val iw_vectorIn = Array.fill(params.max_ntx_nrx)(11)
    val ad_matrixIn = Array.fill(params.max_ntx_nrx, params.max_ntx_nrx)(3)
    val ad_vectorIn = Array.fill(params.max_ntx_nrx)(12)
    val me_vectorOut = Array.fill(params.max_ntx_nrx)(13)

    // Iterate through the tests
    for(t <- 0 until 3)
    {
        // Load the module matrix inputs
        for(i <- 0 until params.max_ntx_nrx) {
            for(j <- 0 until params.max_ntx_nrx) {
                poke( c.io.toChannelEstimator.matrixIn(i)(j).real.raw, conv_double_to_fp(ce_matrixIn(i)(j), params.fix_pt_frac_bits, params.fix_pt_wd) )
                poke( c.io.toChannelEstimator.matrixIn(i)(j).imag.raw, conv_double_to_fp(ce_matrixIn(i)(j), params.fix_pt_frac_bits, params.fix_pt_wd) )
                poke( c.io.toInitializeWeights.matrixIn(i)(j).real.raw, conv_double_to_fp(iw_matrixIn(i)(j), params.fix_pt_frac_bits, params.fix_pt_wd) )
                poke( c.io.toInitializeWeights.matrixIn(i)(j).imag.raw, conv_double_to_fp(iw_matrixIn(i)(j), params.fix_pt_frac_bits, params.fix_pt_wd) )
                poke( c.io.toAdaptiveDecoder.matrixIn(i)(j).real.raw, conv_double_to_fp(ad_matrixIn(i)(j), params.fix_pt_frac_bits, params.fix_pt_wd) )
                poke( c.io.toAdaptiveDecoder.matrixIn(i)(j).imag.raw, conv_double_to_fp(ad_matrixIn(i)(j), params.fix_pt_frac_bits, params.fix_pt_wd) )
            }
        }

        // Load the vectors outputs from the MatrixEngine and the modules
        for(i <- 0 until params.max_ntx_nrx)
        {
            poke( c.io.toChannelEstimator.vectorIn(i).real.raw, conv_double_to_fp(ce_vectorIn(i), params.fix_pt_frac_bits, params.fix_pt_wd) )
            poke( c.io.toChannelEstimator.vectorIn(i).imag.raw, conv_double_to_fp(ce_vectorIn(i), params.fix_pt_frac_bits, params.fix_pt_wd) )
            poke( c.io.toInitializeWeights.vectorIn(i).real.raw, conv_double_to_fp(iw_vectorIn(i), params.fix_pt_frac_bits, params.fix_pt_wd) )
            poke( c.io.toInitializeWeights.vectorIn(i).imag.raw, conv_double_to_fp(iw_vectorIn(i), params.fix_pt_frac_bits, params.fix_pt_wd) )
            poke( c.io.toAdaptiveDecoder.vectorIn(i).real.raw, conv_double_to_fp(ad_vectorIn(i), params.fix_pt_frac_bits, params.fix_pt_wd) )
            poke( c.io.toAdaptiveDecoder.vectorIn(i).imag.raw, conv_double_to_fp(ad_vectorIn(i), params.fix_pt_frac_bits, params.fix_pt_wd) )
            poke( c.io.toMatrixEngine.result(i).real.raw, conv_double_to_fp(me_vectorOut(i), params.fix_pt_frac_bits, params.fix_pt_wd) )
            poke( c.io.toMatrixEngine.result(i).imag.raw, conv_double_to_fp(me_vectorOut(i), params.fix_pt_frac_bits, params.fix_pt_wd) )
        }

        // Set the active module
        poke(c.io.reqChannelEstimator, if(t == 0) 1 else 0)
        poke(c.io.reqInitializeWeights, if(t == 1) 1 else 0)
        poke(c.io.reqAdaptiveDecoder, if(t == 2) 1 else 0)

        // Clock the module
        step(1)

        val testMatrix = t match {
          case 0 => ce_matrixIn
          case 1 => iw_matrixIn
          case 2 => ad_matrixIn
        }
        val testVector = t match {
          case 0 => ce_vectorIn
          case 1 => iw_vectorIn
          case 2 => ad_vectorIn
        }

        // Check the inputs to the matrix engine
        for(i <- 0 until params.max_ntx_nrx)
        {
            expect( c.io.toMatrixEngine.vectorIn(i).real.raw, conv_double_to_fp(testVector(i), params.fix_pt_frac_bits, params.fix_pt_wd) )

            for(j <- 0 until params.max_ntx_nrx)
            {
                expect( c.io.toMatrixEngine.matrixIn(i)(j).real.raw, conv_double_to_fp(testMatrix(i)(j), params.fix_pt_frac_bits, params.fix_pt_wd) )
                expect( c.io.toMatrixEngine.matrixIn(i)(j).imag.raw, conv_double_to_fp(testMatrix(i)(j), params.fix_pt_frac_bits, params.fix_pt_wd) )
            }
        }

        // Check the result going back to the module
        for(i <- 0 until params.max_ntx_nrx)
        {
            expect( c.io.toChannelEstimator.result(i).real.raw, conv_double_to_fp(me_vectorOut(i), params.fix_pt_frac_bits, params.fix_pt_wd) )
            expect( c.io.toChannelEstimator.result(i).imag.raw, conv_double_to_fp(me_vectorOut(i), params.fix_pt_frac_bits, params.fix_pt_wd) )
            expect( c.io.toInitializeWeights.result(i).real.raw, conv_double_to_fp(me_vectorOut(i), params.fix_pt_frac_bits, params.fix_pt_wd) )
            expect( c.io.toInitializeWeights.result(i).imag.raw, conv_double_to_fp(me_vectorOut(i), params.fix_pt_frac_bits, params.fix_pt_wd) )
            expect( c.io.toAdaptiveDecoder.result(i).real.raw, conv_double_to_fp(me_vectorOut(i), params.fix_pt_frac_bits, params.fix_pt_wd) )
            expect( c.io.toAdaptiveDecoder.result(i).imag.raw, conv_double_to_fp(me_vectorOut(i), params.fix_pt_frac_bits, params.fix_pt_wd) )
        }
    }
}


