# Parameters for LMS Decoder
fix_pt_wd ?= 24
fix_pt_exp ?= 12
max_ntx ?= 4
max_nrx ?= 4
T ?= MatrixEngine
testDir ?= snr_27dB

vlsi:
	mkdir -p vlsi/generated-src
	cd sbt; sbt "run -params_$(fix_pt_wd)_$(fix_pt_exp)_$(max_ntx)_$(max_nrx) --backend v --targetDir ../vlsi/generated-src"

default:
	mkdir -p test/generated-src
	cd sbt; sbt "run --genHarness --compile --targetDir ../test/generated-src"

test:
	mkdir -p test/generated-src
	cd sbt; sbt "run -params_$(fix_pt_wd)_$(fix_pt_exp)_$(max_ntx)_$(max_nrx) -testModule_$(T) -testDir_$(testDir) --test --genHarness --compile --targetDir ../test/generated-src"

debug:
	mkdir -p test/generated-src
	cd sbt; sbt "run -params_$(fix_pt_wd)_$(fix_pt_exp)_$(max_ntx)_$(max_nrx) --test --genHarness --compile --debug --vcd --targetDir ../test/generated-src"

clean:
	rm -rf sbt/target sbt/project/target vlsi/generated-src/* test/generated-src/*

.PHONY: vlsi test

