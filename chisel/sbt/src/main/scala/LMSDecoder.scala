package Work

import Chisel._
import Node._
import scala.collection.mutable.HashMap
import scala.math._
import LMSConstants._


// Define the top-level I/O interface for the LMS Decoder
// It contains the coordinates, the rotation mode and the valid bit
class LMSDecoderIO(implicit params: LMSParams) extends Bundle()
{
    // The DecoupledIO interface contains 3 fields: ready (in), valid (out) and bits (out)

    // From the host to the decoder
    val addr = UInt(width = params.addr_wd).asInput
    val data_h2d = Decoupled( new ComplexSInt(w = params.samp_wd) ).flip()

    // From the decoder to the host
    val data_d2h = Decoupled( UInt(width = params.symbol_wd) )

    override def clone: this.type = { new LMSDecoderIO().asInstanceOf[this.type]; }
}


// The main LMS Decoder module definition
class LMSDecoder(paramsIn: LMSParams) extends Module
{
    // Create the params and I/O
    implicit val params = paramsIn
    val io = new LMSDecoderIO()

    // Create the write enable logic
    val mem_address = io.addr(params.addr_wd-3, 0)
    val config_reg_we = ( io.addr(params.addr_wd-1, params.addr_wd-2) === UInt(0) ) & io.data_h2d.valid
    val train_mem_we = ( io.addr(params.addr_wd-1, params.addr_wd-2) === UInt(1) ) & io.data_h2d.valid
    val rx_data_queue_we = ( io.addr(params.addr_wd-1, params.addr_wd-2) === UInt(2) ) & io.data_h2d.valid

    // Create the queues, registers and memory storage
    val train_mem = Mem(new ComplexSInt(w = params.samp_wd), params.max_train_len)
    val rx_data_queue = Module(new Queue(new ComplexSInt(w = params.samp_wd), entries = params.fifo_len))
    val decoded_data_queue = Module(new Queue(UInt(width = params.symbol_wd), entries = params.fifo_len))

    // Wire up the write interface to the registers
    when(config_reg_we) {
        when(mem_address === UInt(0)) {
            ConfigRegisters.ntx := io.data_h2d.bits.real(REG_WD-1, 0)
        }
        .elsewhen(mem_address === UInt(1)) {
            ConfigRegisters.nrx := io.data_h2d.bits.real(REG_WD-1, 0)
        }
        .elsewhen(mem_address === UInt(2)) {
            ConfigRegisters.train_len := io.data_h2d.bits.real(REG_WD-1, 0)
        }
        .elsewhen(mem_address === UInt(3)) {
            ConfigRegisters.modulation := io.data_h2d.bits.real(REG_WD-1, 0)
        }
        .elsewhen(mem_address === UInt(4)) {
            ConfigRegisters.snr := io.data_h2d.bits.real(REG_WD-1, 0)
        }
        .elsewhen(mem_address === UInt(5)) {
            ConfigRegisters.start := io.data_h2d.bits.real(0)
        }
    }

    // Wire up the training sequence memory
    when(train_mem_we) {
        train_mem(mem_address) := io.data_h2d.bits
    }
    // TODO: add the read interface

    // Wire up the RX data queue
    rx_data_queue.io.enq.bits := io.data_h2d.bits
    rx_data_queue.io.enq.valid := rx_data_queue_we
    io.data_h2d.ready := rx_data_queue.io.enq.ready
    // TODO: add the read interface

    // Wire up the Decoded Data Memory
    decoded_data_queue.io.deq <> io.data_d2h
    // TODO: add the write interface

    // Create the matrix engine
    val matrixEngine = Module(new MatrixEngine())

    // Create the channel estimator
    val channelEstimator = Module(new ChannelEstimator())

    // Create the module to compute the initial weights
    val initializeWeights = Module(new InitializeWeights())

    // Create the actual adaptive decoder
    val decoder = Module(new AdaptiveDecoder())
}

