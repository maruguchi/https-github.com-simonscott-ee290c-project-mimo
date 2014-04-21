package Work

import Chisel._
import Node._
import FixedPoint._
import LMSConstants._
import scala.math._


// High-level compilation parameters
// TODO: this must be parameterized based on max packet len, etc
case class LMSParams()
{
    // Simulation parameters
    val max_ntx = 4
    val max_nrx = 4
    val max_ntx_nrx = max(max_ntx, max_nrx)
    val max_train_len = 8
    val fifo_len = 16
    val num_registers = 6

    // Bit widths
    val addr_wd = log2Up( max(num_registers, 6) )
    val samp_wd = 10
    val symbol_wd = log2Up(64)
    val fix_pt_wd = 16
    val fix_pt_exp = 8
}


// The complex type (signed integer)
class ComplexSInt(w: Int, e: Int = 0) extends Bundle()
{
    val real = SInt(width = w)
    val imag = SInt(width = w)

    override def clone: this.type = { new ComplexSInt(w, e).asInstanceOf[this.type]; }
}

// The complex type (signed fixed-point integer)
class ComplexSFix(w: Int, e: Int = -999) extends Bundle()
{
    val e1 = if(e == -999) w else e
    val real = SFix(exp = e1, width = w)
    val imag = SFix(exp = e1, width = w)

    override def clone: this.type = { new ComplexSFix(w, e1).asInstanceOf[this.type]; }
}


// Constants
object LMSConstants
{
    // Modulation options
    val MOD_BPSK = UInt(0)
    val MOD_QPSK = UInt(1)
    val MOD_16QAM = UInt(2)
    val MOD_64QAM = UInt(3)

    // Matrix Engine operations
    val MAT_MAT_MULT = UInt(0)
    val CONJ_TRANSP = UInt(1)

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
    val snr = Reg(init = UInt(0, width = REG_WD))
    val start = Reg(init = Bool(false))
}

