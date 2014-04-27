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
        io.toMatrixEngine := io.toChannelEstimator
    }
    .elsewhen(io.reqInitializeWeights) {
        io.toMatrixEngine := io.toInitializeWeights
    }
    .otherwise {
        io.toMatrixEngine := io.toAdaptiveDecoder
    }
}


