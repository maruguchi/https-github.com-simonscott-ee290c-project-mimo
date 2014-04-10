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
    // From the host to the decoder

    val addr = UInt(width = params.addr_wd)
    val data_h2d = new Complex(w = params.samp_wd)
    val we = Bool()

    // From the decoder to the host
    // The "data_d2h" interface is used for reading the Decoded Data Queue
    // It contains 3 fields: ready (in), valid (out) and bits (out)

    val rx_buf_ready = Bool()
    val data_d2h = Decoupled( new Complex(w = params.symbol_wd) )

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
    val config_reg_we = ( io.addr(params.addr_wd-1, params.addr_wd-2) === UInt(0) ) & io.we
    val train_mem_we = ( io.addr(params.addr_wd-1, params.addr_wd-2) === UInt(1) ) & io.we
    val rx_data_queue_we = ( io.addr(params.addr_wd-1, params.addr_wd-2) === UInt(2) ) & io.we

    // Create the queues, registers and memory storage
    val train_mem = Mem(new Complex(w = params.samp_wd), params.max_train_len)
    val rx_data_queue = Queue(new Complex(w = params.samp_wd), entries = params.fifo_len)
    val decoded_data_queue = Queue(new Complex(w = params.samp_wd), entries = params.fifo_len)

    // Wire up the write interface to the registers
    when(config_reg_we) {
        when(mem_address === UInt(0)) {
            ConfigRegisters.ntx := io.data_h2d.real(REG_WD-1, 0)
        }
        .elsewhen(mem_address === UInt(1)) {
            ConfigRegisters.ntx := io.data_h2d.real(REG_WD-1, 0)
        }
        .elsewhen(mem_address === UInt(2)) {
            ConfigRegisters.nrx := io.data_h2d.real(REG_WD-1, 0)
        }
        .elsewhen(mem_address === UInt(3)) {
            ConfigRegisters.train_len := io.data_h2d.real(REG_WD-1, 0)
        }
        .elsewhen(mem_address === UInt(4)) {
            ConfigRegisters.modulation := io.data_h2d.real(REG_WD-1, 0)
        }
        .elsewhen(mem_address === UInt(5)) {
            ConfigRegisters.start := io.data_h2d.real(0)
        }
    }

    // Wire up the training sequence memory
    when(train_mem_we) {
        train_mem(mem_address) := io.data_h2d
    }
    // TODO: add the read interface

    // Wire up the RX data queue
    rx_data_queue.io.enq.bits := io.data_h2d
    rx_data_queue.io.enq.valid := rx_data_queue_we
    io.rx_buf_ready := rx_data_queue.io.enq.ready
    // TODO: add the read interface

    // Wire up the Decoded Data Memory
    decoded_data_queue.io.denq <> io.data_d2h
    // TODO: add the write interface

    // Create the channel estimator
    val channelEstimator = Module(new ChannelEstimator())
}

