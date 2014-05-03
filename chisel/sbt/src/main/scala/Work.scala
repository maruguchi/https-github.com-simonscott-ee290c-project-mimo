package Work

import Chisel._
  
object Work {

  def main(args: Array[String]):Unit = {

    // Parse parameters: set LMS params
    // TODO: actually parse the parameters
    val params = new LMSParams()

    // Parse parameters: figure out which module to test
    val test_module = """-testmodule_(.*)""".r.findFirstMatchIn(args(1))

    // If no module defined, it means we don't want to perform a test. Just generate Verilog
    if(!test_module.isDefined)
        chiselMain( args.slice(1, args.length), () => Module(new LMSDecoder(params)) ) 

    // Else perform tests
    else
    {
        // Test the appropriate module
        test_module.get.group(1) match {
            case "ChannelEstimatorEngine" => 
                chiselMainTest( args.slice(2, args.length), () => Module(new ChannelEstimatorEngine()(params)) ) {
                    c => new ChannelEstimatorEngineTests(c, params) }

            case "MatrixEngine" =>
                chiselMainTest( args.slice(2, args.length), () => Module(new MatrixEngine()(params)) ) {
                    c => new MatrixEngineTests(c, params) }
     
            case "AdaptiveDecoder" =>
                chiselMainTest( args.slice(2, args.length), () => Module(new AdaptiveDecoderWithMatrixEng()(params)) ) {
                    c => new AdaptiveDecoderTests(c, params) }

            case "MatrixArbiter" =>
                chiselMainTest( args.slice(2, args.length), () => Module(new MatrixArbiter()(params)) ) {
                    c => new MatrixArbiterTests(c, params) }

            case "FixDivision" => 
                chiselMainTest( args.slice(2, args.length), () => Module(new FixDivision()(params)) ) {
                    c => new FixDivisionTests(c, params) }

            case "Mat2Inverse" => 
                chiselMainTest( args.slice(2, args.length), () => Module(new Mat2Inverse()(params)) ) {
                    c => new Mat2InverseTests(c, params) }

            case "Mat3Inverse" => 
                chiselMainTest( args.slice(2, args.length), () => Module(new Mat3Inverse()(params)) ) {
                    c => new Mat3InverseTests(c, params) }

            case "Mat4Inverse" => 
                chiselMainTest( args.slice(2, args.length), () => Module(new Mat4Inverse()(params)) ) {
                    c => new Mat4InverseTests(c, params) }

            case "LMSDecoder" => 
                chiselMainTest( args.slice(2, args.length), () => Module(new LMSDecoder(params)) ) {
                    c => new LMSDecoderTester(c) }
        }
    }
  }
}   
