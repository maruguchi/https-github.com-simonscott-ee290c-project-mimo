package Work

import Chisel._
import Node._
import scala.collection.mutable.HashMap
import scala.math._
import LMSConstants._
import ComplexMathFunctions._


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

    // Create the state variables for the control state machine
    val st_RESET :: st_EST_CH :: st_INIT_W :: st_DECODE :: Nil = Enum(UInt(), 4)
    val state = Reg(init = st_RESET)


    // ***** Create and wire up the memories *****

    // Create the write enable logic
    val config_mem_address = io.addr(params.reg_addr_wd-1, 0)
    val train_mem_address = io.addr(params.train_addr_wd-1, 0)
    val config_reg_we = ( io.addr(params.addr_wd-1, params.addr_wd-2) === UInt(0) ) & io.data_h2d.valid
    val train_mem_we = ( io.addr(params.addr_wd-1, params.addr_wd-2) === UInt(1) ) & io.data_h2d.valid
    val rx_data_queue_we = ( io.addr(params.addr_wd-1, params.addr_wd-2) === UInt(2) ) & io.data_h2d.valid

    // Create the queues and memory storage
    val train_mem = Mem(Bits(width=params.max_ntx_nrx*2*params.samp_wd), params.max_train_len, seqRead = false)
    val rx_data_queue = Module(new Queue(Bits(width=params.max_ntx_nrx*2*params.samp_wd), entries = params.fifo_len))
    val decoded_data_queue = Module(new Queue(Bits(width=params.max_ntx_nrx*params.symbol_wd), entries = params.fifo_len))

    // Create the configuration registers
    val ntx = Reg(init = UInt(0, width = REG_WD))
    val nrx = Reg(init = UInt(0, width = REG_WD))
    val train_len = Reg(init = UInt(0, width = REG_WD))
    val modulation = Reg(init = UInt(0, width = REG_WD))
    val snr = Reg(init = UInt(0, width = REG_WD))
    val start = Reg(init = Bool(false))

    // Wire up the write interface to the registers
    when(config_reg_we) {
        when(config_mem_address === UInt(0)) {
            ntx := io.data_h2d.bits(0).real.raw(REG_WD-1, 0)
        }
        when(config_mem_address === UInt(1)) {
            nrx := io.data_h2d.bits(0).real.raw(REG_WD-1, 0)
        }
        when(config_mem_address === UInt(2)) {
            train_len := io.data_h2d.bits(0).real.raw(REG_WD-1, 0)
        }
        when(config_mem_address === UInt(3)) {
            modulation := io.data_h2d.bits(0).real.raw(REG_WD-1, 0)
        }
        when(config_mem_address === UInt(4)) {
            snr := io.data_h2d.bits(0).real.raw(REG_WD-1, 0)
        }
        when(config_mem_address === UInt(5) && (state === st_RESET || state === st_DECODE)) {
            start := io.data_h2d.bits(0).real.raw(0)
        }
    }

    val data_h2d_fv = vecToBits(io.data_h2d.bits, params.max_ntx_nrx, params.samp_wd)

    // Wire up the training sequence memory
    when(train_mem_we) {
        train_mem( train_mem_address ) := data_h2d_fv
    }

    // Wire up the RX data queue
    rx_data_queue.io.enq.bits := data_h2d_fv
    rx_data_queue.io.enq.valid := rx_data_queue_we
    io.data_h2d.ready := rx_data_queue.io.enq.ready
    val rx_data_queue_vecOut =  bitsToVec(rx_data_queue.io.deq.bits, params.max_ntx_nrx, params.samp_wd, params.samp_exp)

    // Wire up the Decoded Data Memory
    io.data_d2h.bits := Vec.fill(params.max_ntx_nrx){UInt(width = params.symbol_wd)}.fromBits(decoded_data_queue.io.deq.bits)
    io.data_d2h.valid := decoded_data_queue.io.deq.valid
    decoded_data_queue.io.deq.ready := io.data_d2h.ready


    // ***** Create all the modules *****

    // Create the matrix engine
    val matrixEngine = Module(new MatrixEngine)

    // Create the arbiter to control access to the matrix engine
    val matrixArbiter = Module(new MatrixArbiter)

    // Create the channel estimator
    val channelEstimator = Module(new ChannelEstimator)

    // Create the module to compute the initial weights
    val initializeWeights = Module(new InitializeWeights)

    // Create the actual adaptive decoder
    val adaptiveDecoder = Module(new AdaptiveDecoder)

    
    // ***** Create the control logic to enable/disable the modules *****

    val channelEstimator_en = Bool()
    val channelEstimator_rst = Bool()
    val channelEstimator_done = Bool()
    val initializeWeights_done = Bool()
    val initializeWeights_en = Bool()
    val initializeWeights_rst = Bool()
    val adaptiveDecoder_en = Bool()
    val adaptiveDecoder_resetW = Bool()

    // The RESET state: only enter this upon power-up
    when(state === st_RESET)
    {
        // If host started a new decode
        when(start) {
            state                   := st_EST_CH
            channelEstimator_en     := Bool(true)
        }
        .otherwise {
            channelEstimator_rst    := Bool(true)
        }
    }

    // The ESTIMATE CHANNEL state:
    .elsewhen(state === st_EST_CH)
    {
        start                       := Bool(false)
        channelEstimator_en         := Bool(true)

        when(channelEstimator_done) {
            state                   := st_INIT_W
            initializeWeights_rst   := Bool(true)
        }
    }

    // The INITIALIZE WEIGHTS state:
    .elsewhen(state === st_INIT_W)
    {
        initializeWeights_en        := Bool(true)

        when(initializeWeights_done) {
            state                   := st_DECODE
            adaptiveDecoder_resetW  := Bool(true)
        }
    }

    // The DECODE state:
    .elsewhen(state === st_DECODE)
    {
        // Check if the host has requested a new frame be decoded
        when(start) {
            state                   := st_EST_CH
            channelEstimator_en     := Bool(true)
        }
        .otherwise {
            adaptiveDecoder_en      := Bool(true)
            channelEstimator_rst    := Bool(true)
        }
    }

    // The default state for signals not covered by states above
    .otherwise
    {
        initializeWeights_rst       := Bool(false)
        initializeWeights_en        := Bool(false)
        channelEstimator_rst        := Bool(false)
        channelEstimator_en         := Bool(false)
        adaptiveDecoder_resetW      := Bool(false)
        adaptiveDecoder_en          := Bool(false)
    }
   

    // ***** Wire up all the modules *****

    // Matrix engine
    matrixEngine.io <> matrixArbiter.io.toMatrixEngine

    // Channel estimator
	channelEstimator.io.start               := channelEstimator_en
	channelEstimator.io.rst                 := channelEstimator_rst
	channelEstimator_done                   := channelEstimator.io.done
    channelEstimator.io.Nant                := ntx
	channelEstimator.io.dataIn.valid        := rx_data_queue.io.deq.valid
    matrixArbiter.io.reqChannelEstimator    := channelEstimator_en & (~channelEstimator_done)
	matrixArbiter.io.toChannelEstimator     <> channelEstimator.io.toMatEngine

    val trainMemOut                         = bitsToVec(train_mem( channelEstimator.io.trainAddress ), params.max_ntx_nrx, params.samp_wd, params.samp_exp)
    for(i <- 0 until params.max_ntx_nrx) {
	    channelEstimator.io.trainSequence(i).real := trainMemOut(i).real
	    channelEstimator.io.trainSequence(i).imag := trainMemOut(i).imag
	    channelEstimator.io.dataIn.bits(i).real   := rx_data_queue_vecOut(i).real
	    channelEstimator.io.dataIn.bits(i).imag   := rx_data_queue_vecOut(i).imag
    }

    // Initialize weights
    initializeWeights.io.channelMatrix      <> channelEstimator.io.channelOut
    initializeWeights.io.snr                := snr
    initializeWeights.io.Nant               := nrx
    initializeWeights.io.start              := initializeWeights_en    
    initializeWeights.io.rst                := initializeWeights_rst    
    initializeWeights_done                  := initializeWeights.io.done
    matrixArbiter.io.toInitializeWeights    <> initializeWeights.io.toMatEngine
    matrixArbiter.io.reqInitializeWeights   := initializeWeights_en & (~initializeWeights_done)
    
    // Adaptive decoder
    adaptiveDecoder.io.wSeed                := initializeWeights.io.initialW
    adaptiveDecoder.io.samples.bits         := rx_data_queue_vecOut
    adaptiveDecoder.io.samples.valid        := rx_data_queue.io.deq.valid
    decoded_data_queue.io.enq.bits          := adaptiveDecoder.io.decodedData.bits.toBits
    decoded_data_queue.io.enq.valid         := adaptiveDecoder.io.decodedData.valid
    adaptiveDecoder.io.decodedData.ready    := decoded_data_queue.io.enq.ready
    adaptiveDecoder.io.resetW               := adaptiveDecoder_resetW
    adaptiveDecoder.io.processSamples       := adaptiveDecoder_en
    matrixArbiter.io.reqAdaptiveDecoder     := adaptiveDecoder.io.reqMatEngine
    matrixArbiter.io.toAdaptiveDecoder      <> adaptiveDecoder.io.toMatEngine  

    rx_data_queue.io.deq.ready := (channelEstimator_en & channelEstimator.io.dataIn.ready) |
                                    (adaptiveDecoder_en & adaptiveDecoder.io.samples.ready)
}

