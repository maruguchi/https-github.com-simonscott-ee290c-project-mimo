package Work

import Chisel._
  
object Work {

  def main(args: Array[String]):Unit = {

    // Parse parameters
    val params = new LMSParams()

    // Test the matrix engine
    chiselMainTest( args.slice(1, args.length), () => Module(new LMSDecoder(params)) ) {
        c => new MatrixEngineTests(c) }
  } 

}   
