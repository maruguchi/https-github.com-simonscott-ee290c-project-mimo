package Work

import Chisel._
import Node._
import FixedPoint._
import LMSConstants._
import scala.math._


// High-level compilation parameters
case class LMSParams(fp_bit_wd: Int, fp_exp: Int, m_ntx: Int, m_nrx: Int)
{
    // Simulation parameters
    val max_ntx = m_ntx
    val max_nrx = m_nrx
    val max_ntx_nrx = max(max_ntx, max_nrx)
    val max_train_len = 4
    val fifo_len = 16
    val num_registers = 6
    val mu = 0.01

    // Bit widths
    val reg_addr_wd = log2Up(num_registers)
    val train_addr_wd = log2Up(max_train_len)
    val addr_wd = max(reg_addr_wd, train_addr_wd) + 2

    val samp_wd = 10
    val samp_exp = 4        // top 4 bits of sample is integer part, giving max value of +/- 8.0
    val samp_frac_bits = samp_wd - samp_exp   

    val symbol_wd = log2Up(4)

    val fix_pt_wd = fp_bit_wd       // works well at 24 and 12
    val fix_pt_exp = fp_exp         // 8 bits int, 12 bits of decimal = 0.0002 resolution
    val fix_pt_frac_bits = fix_pt_wd - fix_pt_exp
}


// The complex type (signed integer)
class ComplexSInt(w: Int) extends Bundle()
{
    val real = SInt(width = w)
    val imag = SInt(width = w)

    override def clone: this.type = { new ComplexSInt(w).asInstanceOf[this.type]; }
}

// The complex type (signed fixed-point integer)
class ComplexSFix(w: Int, e: Int = -999) extends Bundle()
{
    val e1 = if(e == -999) w else e
    val real = SFix(exp = e1, width = w)
    val imag = SFix(exp = e1, width = w)

    override def clone: this.type = { new ComplexSFix(w, e).asInstanceOf[this.type]; }
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
    val REG_WD = 10
}

