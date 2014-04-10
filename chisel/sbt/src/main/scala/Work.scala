package Work

import Chisel._
  
object Work {

  def main(args: Array[String]) = {

    // Parse parameters
    val params = new LMSParams()

    // Create new decoder
    val decoder = Module(new LMSDecoder(params))
  } 

}   
