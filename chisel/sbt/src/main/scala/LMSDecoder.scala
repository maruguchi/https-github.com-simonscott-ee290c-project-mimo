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
    val data_h2d = Decoupled( Vec.fill(params.max_ntx_nrx){new ComplexSFix(w=params.samp_wd, e=params.samp_exp)} ).flip()

    // From the decoder to the host
    val data_d2h = Decoupled( Vec.fill(params.max_ntx_nrx){UInt(width = params.symbol_wd)} )

    override def clone: this.type = { new LMSDecoderIO().asInstanceOf[this.type]; }
}


// The main LMS Decoder module definition
class LMSDecoder(paramsIn: LMSParams) extends Module
{
    // Create the params and I/O
    implicit val params = paramsIn
    val io = new LMSDecoderIO()


    // ***** Create and wire up the memories *****

    // Create the write enable logic
    val mem_address = io.addr(params.addr_wd-3, 0)
    val config_reg_we = ( io.addr(params.addr_wd-1, params.addr_wd-2) === UInt(0) ) & io.data_h2d.valid
    val train_mem_we = ( io.addr(params.addr_wd-1, params.addr_wd-2) === UInt(1) ) & io.data_h2d.valid
    val rx_data_queue_we = ( io.addr(params.addr_wd-1, params.addr_wd-2) === UInt(2) ) & io.data_h2d.valid

    // Create the queues, registers and memory storage
    val train_mem = Mem(Vec.fill(params.max_ntx_nrx){new ComplexSFix(w=params.samp_wd, e=params.samp_exp)}, params.max_train_len)
    val rx_data_queue = Module(new Queue(Vec.fill(params.max_ntx_nrx){new ComplexSFix(w=params.samp_wd, e=params.samp_exp)}, entries = params.fifo_len))
    val decoded_data_queue = Module(new Queue(Vec.fill(params.max_ntx_nrx){UInt(width = params.symbol_wd)}, entries = params.fifo_len))

    // Wire up the write interface to the registers
    when(config_reg_we) {
        when(mem_address === UInt(0)) {
            ConfigRegisters.ntx := io.data_h2d.bits(0).real.raw(REG_WD-1, 0)
        }
        .elsewhen(mem_address === UInt(1)) {
            ConfigRegisters.nrx := io.data_h2d.bits(0).real.raw(REG_WD-1, 0)
        }
        .elsewhen(mem_address === UInt(2)) {
            ConfigRegisters.train_len := io.data_h2d.bits(0).real.raw(REG_WD-1, 0)
        }
        .elsewhen(mem_address === UInt(3)) {
            ConfigRegisters.modulation := io.data_h2d.bits(0).real.raw(REG_WD-1, 0)
        }
        .elsewhen(mem_address === UInt(4)) {
            ConfigRegisters.snr := io.data_h2d.bits(0).real.raw(REG_WD-1, 0)
        }
        .elsewhen(mem_address === UInt(5)) {
            ConfigRegisters.start := io.data_h2d.bits(0).real.raw(0)
        }
    }

    // Wire up the training sequence memory
    when(train_mem_we) {
        train_mem( mem_address(params.train_addr_wd,0) ) := io.data_h2d.bits
    }

    // Wire up the RX data queue
    rx_data_queue.io.enq.bits := io.data_h2d.bits
    rx_data_queue.io.enq.valid := rx_data_queue_we
    io.data_h2d.ready := rx_data_queue.io.enq.ready

    // Wire up the Decoded Data Memory
    decoded_data_queue.io.deq <> io.data_d2h


    // ***** Create all the modules *****

    // Create the matrix engine
    val matrixEngine = Module(new MatrixEngine)

    // Create the arbiter to control access to the matrix engine
    val matrixArbiter = Module(new MatrixArbiter)

    // Create the channel estimator
    val channelEstimator = Module(new ChannelEstimator)

    // Create the module to compute the initial weights
    // val initializeWeights = Module(new InitializeWeights)

    // Create the actual adaptive decoder
    val adaptiveDecoder = Module(new AdaptiveDecoder)

    
    // ***** Create the control logic to enable/disable the modules *****

    // Lots of TODO
    val adaptiveDecoder_en = Bool(true)
    val adaptiveDecoder_resetW = Bool(false)
    val channelEstimator_en = Bool(true)
    val channelEstimator_reset = Bool(false)
    val channelEstimator_done = Bool()
    

    // ***** Wire up all the modules *****

    // Matrix engine
    matrixEngine.io <> matrixArbiter.io.toMatrixEngine

    // Adaptive decoder
    //adaptiveDecoder.io.wSeed <>
    adaptiveDecoder.io.wSeed            <> channelEstimator.io.channelOut  // This line is wrong! Remove it! Just here for testing!
    adaptiveDecoder.io.samples.bits     := rx_data_queue.io.deq.bits
    adaptiveDecoder.io.samples.valid    := rx_data_queue.io.deq.valid
    adaptiveDecoder.io.decodedData      <> decoded_data_queue.io.enq
    adaptiveDecoder.io.resetW           := adaptiveDecoder_resetW
    adaptiveDecoder.io.processSamples   := adaptiveDecoder_en
    matrixArbiter.io.reqAdaptiveDecoder := adaptiveDecoder.io.reqMatEngine
    matrixArbiter.io.toAdaptiveDecoder  <> adaptiveDecoder.io.toMatEngine  

    // Channel estimator
	channelEstimator.io.start           := channelEstimator_en
	channelEstimator.io.rst             := channelEstimator_reset
	channelEstimator_done               := channelEstimator.io.done
	channelEstimator.io.trainSequence   := train_mem( channelEstimator.io.trainAddress )
	channelEstimator.io.dataIn.bits     := rx_data_queue.io.deq.bits
	channelEstimator.io.dataIn.valid    := rx_data_queue.io.deq.valid
    matrixArbiter.io.reqChannelEstimator := channelEstimator_en & (~channelEstimator_done)
	matrixArbiter.io.toChannelEstimator <> channelEstimator.io.toMatEngine

    rx_data_queue.io.deq.ready := (channelEstimator_en & channelEstimator.io.dataIn.ready) |
                                    (adaptiveDecoder_en & adaptiveDecoder.io.samples.ready)

    // Initialize weights
    // 	? := channelEstimator.io.channelOut
    // TODO

}

