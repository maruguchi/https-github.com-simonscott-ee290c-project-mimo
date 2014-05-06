package Work

import Chisel._
  
object Work {

  def main(args: Array[String]):Unit = {

    // Parse parameters: set LMS params
    val param_str = """-params_(.*)_(.*)_(.*)_(.*)""".r.findFirstMatchIn(args(0))
    require(param_str.isDefined, "First argument must be -param_w_e_t_r")
    val params = new LMSParams(param_str.get.group(1).toInt, param_str.get.group(2).toInt, param_str.get.group(3).toInt, param_str.get.group(4).toInt)

    // Parse parameters: figure out which module to test
    val test_module = """-testModule_(.*)""".r.findFirstMatchIn(args(1))

    // Parse parameters: determine which test directory to use
    val testDir = """-testDir_(.*)""".r.findFirstMatchIn(args(2))

    // If no module defined, it means we don't want to perform a test. Just generate Verilog
    if(!test_module.isDefined)
        chiselMain( args.slice(1, args.length), () => Module(new LMSDecoder(params)) ) 

    // Else perform tests
    else
    {
        // Test the appropriate module
        test_module.get.group(1) match {
            case "ChannelEstimatorEngine" => 
                chiselMainTest( args.slice(3, args.length), () => Module(new ChannelEstimatorEngine()(params)) ) {
                    c => new ChannelEstimatorEngineTests(c, params) }

            case "MatrixEngine" =>
                chiselMainTest( args.slice(3, args.length), () => Module(new MatrixEngine()(params)) ) {
                    c => new MatrixEngineTests(c, params) }
     
            case "AdaptiveDecoder" =>
                chiselMainTest( args.slice(3, args.length), () => Module(new AdaptiveDecoderWithMatrixEng()(params)) ) {
                    c => new AdaptiveDecoderTests(c, params) }

            case "MatrixArbiter" =>
                chiselMainTest( args.slice(3, args.length), () => Module(new MatrixArbiter()(params)) ) {
                    c => new MatrixArbiterTests(c, params) }

            case "FixDivision" => 
                chiselMainTest( args.slice(3, args.length), () => Module(new FixDivision()(params)) ) {
                    c => new FixDivisionTests(c, params) }

            case "Mat2Inverse" => 
                chiselMainTest( args.slice(3, args.length), () => Module(new Mat2Inverse()(params)) ) {
                    c => new Mat2InverseTests(c, params) }

            case "Mat3Inverse" => 
                chiselMainTest( args.slice(3, args.length), () => Module(new Mat3Inverse()(params)) ) {
                    c => new Mat3InverseTests(c, params) }

            case "Mat4Inverse" => 
                chiselMainTest( args.slice(3, args.length), () => Module(new Mat4InverseEngine()(params)) ) {
                    c => new Mat4InverseTests(c, params) }

            case "InitializeWeightsEngine" => 
                chiselMainTest( args.slice(3, args.length), () => Module(new InitializeWeightsEngine()(params)) ) {
                    c => new InitializeWeightsEngineTests(c, params) }

            case "LMSDecoder" => 
                chiselMainTest( args.slice(3, args.length), () => Module(new LMSDecoder(params)) ) {
                    c => new LMSDecoderTester(c, testDir.get.group(1)) }
        }
    }
  }
}   
