package Work

import Chisel._
import Node._
import scala.collection.mutable._
import scala.math._
import LMSConstants._
import ComplexMathFunctions._

class LMSDecoderTester(c: LMSDecoder) extends Tester(c)
{

    //****** FILE PARSING ******

    // Read in bus commands for config registers/training memory
    // Very simple syntax and parsing. Supports writes only, no reads
    // Form for config registers: [cycle=Int],[address=Int],[writeData=Int]
    // Example: 3,1,4
    // Form for training memory: [cycle=Int],[address=Int],[train_ant0_real=Float],[train_ant0_imag=Float],[train_ant1_real=Float], etc
    // Example: 6,20,1.26,-2.71,0.51
    val configTrainBusCommands = new HashMap[BigInt,Array[Double]]()
    for (line <- scala.io.Source.fromFile("../test/snr_27dB/configTrainBusCmds.txt").getLines()) {
        val command = line.split(",").map(_.toDouble)
        configTrainBusCommands(command(0).toInt) = command.drop(1)
    }

    // Read in bus commands for the receive data queue
    // Very simple syntax and parsing. Supports writes only.
    // Each line contains the samples at a single time for all the receive antennas
    // Form: [wait until this cycle=Int],[sample_ant0_real=Float],[sample_ant0_imag=Float],[sample_ant1_real=Float], etc
    // Example:
    // 10,1.26,-2.71,0.51,...
    val receiveDataBusCommands = new ArrayBuffer[Array[Double]]
    for (line <- scala.io.Source.fromFile("../test/snr_27dB/receiveData.txt").getLines()) {
        val command = line.split(",").map(_.toDouble)
        receiveDataBusCommands += command
    }

    // Read in bus commands for the decoded data queue
    // Very simple syntax and parsing. Supports reads only.
    // Each line contains the decoded data symbols that are expected at a single instance of time
    // Form: [ant0_symbol=Int],[ant1_symbol=Int],[ant2_symbol=Int],[ant3_symbol=Int]
    // Example:
    // 1,0,2,3
    val decodedDataBusCommands = new ArrayBuffer[Array[Int]]
    for (line <- scala.io.Source.fromFile("../test/snr_27dB/decodedData.txt").getLines()) {
        val command = line.split(",").map(_.toInt)
        decodedDataBusCommands += command
    }

    // Number of cycles to run test
    val cycles = 80 // 23

    var num_reads = 0
    var num_writes = 0
    var num_sym_errors = 0


    //****** RUN TESTS ****** 

    for (cycle <- 0 until cycles)
    {
        // Debug output for this cycle
        peek(c.state)
        peekAt(c.train_mem, 0)
        peek(c.channelEstimator.io.trainAddress)
        peek(c.initializeWeights.io.done)
/*
        // Training sequence going into channel estimator
        for(i <- 0 until c.params.max_ntx_nrx) {
            val re = conv_fp_to_double(peek(c.channelEstimator.io.trainSequence(i).real.raw), c.params.samp_frac_bits, c.params.samp_wd)
            val im = conv_fp_to_double(peek(c.channelEstimator.io.trainSequence(i).imag.raw), c.params.samp_frac_bits, c.params.samp_wd)
            println(s"[$re , $im]")
        }
*/
        // Samples going into adaptive decoder
        for(i <- 0 until c.params.max_ntx_nrx) {
            val re = conv_fp_to_double(peek(c.initializeWeights.io.channelMatrix(0)(i).real.raw), c.params.fix_pt_frac_bits, c.params.fix_pt_wd)
            val im = conv_fp_to_double(peek(c.initializeWeights.io.channelMatrix(0)(i).imag.raw), c.params.fix_pt_frac_bits, c.params.fix_pt_wd)
            println(s"[$re , $im]")
        }

        // First row of inverse(H x H_hermitian + 1/SNR matrix)
        for(i <- 0 until c.params.max_ntx_nrx) {
            val re = conv_fp_to_double(peek(c.initializeWeights.inverse(0)(i).real.raw), c.params.fix_pt_frac_bits, c.params.fix_pt_wd)
            val im = conv_fp_to_double(peek(c.initializeWeights.inverse(0)(i).imag.raw), c.params.fix_pt_frac_bits, c.params.fix_pt_wd)
            println(s"[$re , $im]")
        }

        // First row of W seed matrix going into adaptive decoder
        for(i <- 0 until c.params.max_ntx_nrx) {
            val re = conv_fp_to_double(peek(c.initializeWeights.io.initialW(0)(i).real.raw), c.params.fix_pt_frac_bits, c.params.fix_pt_wd)
            val im = conv_fp_to_double(peek(c.initializeWeights.io.initialW(0)(i).imag.raw), c.params.fix_pt_frac_bits, c.params.fix_pt_wd)
            println(s"[$re , $im]")
        }

        // Check for config or train mem write
        if(configTrainBusCommands.contains(cycle))
        {
            val command = configTrainBusCommands(cycle)
            poke(c.io.addr, command(0).toInt)

            // If a config register write
            if(command.length == 2)
                poke(c.io.data_h2d.bits(0).real.raw, command(1).toInt)

            // Else a train mem write
            else {
                for(i <- 0 until (command.length-1)/2) {
                    poke(c.io.data_h2d.bits(i).real.raw, conv_double_to_fp(command(i*2 + 1), c.params.samp_frac_bits, c.params.samp_wd))
                    poke(c.io.data_h2d.bits(i).imag.raw, conv_double_to_fp(command(i*2 + 2), c.params.samp_frac_bits, c.params.samp_wd))
                }
            }

            poke(c.io.data_h2d.valid, 1)
        }

        // Check for sample write
        else if(num_writes < receiveDataBusCommands.length && cycle >= receiveDataBusCommands(num_writes)(0).toInt)
        {
            val receiveSamples = receiveDataBusCommands(num_writes).drop(1)
            
            if(peek(c.io.data_h2d.ready) == 1)
            {
                for(i <- 0 until receiveSamples.length/2) {
                    poke(c.io.data_h2d.bits(i).real.raw, conv_double_to_fp(receiveSamples(i*2 + 0), c.params.samp_frac_bits, c.params.samp_wd))
                    poke(c.io.data_h2d.bits(i).imag.raw, conv_double_to_fp(receiveSamples(i*2 + 1), c.params.samp_frac_bits, c.params.samp_wd))
                }

                poke(c.io.addr, pow(2, c.params.addr_wd-1).toInt)
                poke(c.io.data_h2d.valid, 1)
                num_writes += 1
            }
            else
                poke(c.io.data_h2d.valid, 0)
        }

        // Else we are not writing this cycle, so set valid to low
        else {
            poke(c.io.data_h2d.valid, 0)
        }

        // Check for read of the decoded symbols
        if(peek(c.io.data_d2h.valid) == 1)
        {
            val expectedSymbols = decodedDataBusCommands(num_reads)

            for(i <- 0 until expectedSymbols.length)
            {
                val decodedSymbol = peek(c.io.data_d2h.bits(i))

                if(decodedSymbol == expectedSymbols(i))
                    println(s"Correctly decoded symbol number $num_reads:$i : $decodedSymbol")
                else {
                    println(s"ERROR: incorrectly decoded symbol. Expected: ${expectedSymbols(i)} Got: $decodedSymbol")
                    num_sym_errors += 1
                }
            }
            poke(c.io.data_d2h.ready, 1)
            num_reads += 1
        }

        // Else we are not reading this cycle, so set ready to low
        else {
            poke(c.io.data_d2h.ready, 0)
        }

        // Next clock cycle
        step(1);
    }

    val ber = num_sym_errors.toDouble / (num_reads * 4.0)
    println("---------------------------------------------------------")
    println(s"Test complete. ${num_sym_errors} out of ${num_reads*4} symbols incorrect. BER = $ber")
}
