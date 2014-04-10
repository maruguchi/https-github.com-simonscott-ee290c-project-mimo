package Work

import Chisel._
import Node._
import LMSConstants._

// High-level compilation parameters
// TODO: this must be parameterized based on max packet len, etc
case class LMSParams()
{
    val addr_wd = 16
    val samp_wd = 16
    val symbol_wd = 16
    val max_ntx = 4
    val max_nrx = 4
    val max_train_len = 8
    val fifo_len = 16
}


// The complex type
class Complex(w: Int) extends Bundle() {
    val real = SInt(width = w)
    val imag = SInt(width = w)

    override def clone: this.type = { new Complex(w).asInstanceOf[this.type]; }
}


// Constants
object LMSConstants
{
    // Modulation options
    val MOD_BPSK = UInt(0)
    val MOD_QPSK = UInt(1)
    val MOD_16QAM = UInt(2)
    val MOD_64QAM = UInt(3)

    // Fixed bit-widths (i.e. not parameterizable)
    val REG_WD = 4
}


// Configuration registers
// Constants
object ConfigRegisters
{
    val ntx = Reg(init = UInt(0, width = REG_WD))
    val nrx = Reg(init = UInt(0, width = REG_WD))
    val train_len = Reg(init = UInt(0, width = REG_WD))
    val modulation = Reg(init = UInt(0, width = REG_WD))
    val start = Reg(init = Bool(false))
}

