# Parameters for CORDIC
# K is x,y bitwidth, L is z bitwidth, R is unrolling factor
K ?= 16
L ?= 16
R ?= 0.0
T ?= MatrixEngine

vlsi:
	mkdir -p vlsi/generated-src
	cd sbt; sbt "run -params_$(K)_$(L)_$(R) --backend v --targetDir ../vlsi/generated-src"

default:
	mkdir -p test/generated-src
	cd sbt; sbt "run --genHarness --compile --targetDir ../test/generated-src"

test:
	mkdir -p test/generated-src
	cd sbt; sbt "run -params_$(K)_$(L)_$(R) -testmodule_$(T) --test --genHarness --compile --targetDir ../test/generated-src"

debug:
	mkdir -p test/generated-src
	cd sbt; sbt "run -params_$(K)_$(L)_$(R) --test --genHarness --compile --debug --vcd --targetDir ../test/generated-src"

clean:
	rm -rf sbt/target sbt/project vlsi/generated-src/* test/generated-src/*

.PHONY: vlsi test

