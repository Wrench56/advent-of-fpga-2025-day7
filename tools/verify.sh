#!/bin/sh

set -e

mkdir -p "artifacts/"
yosys -p "read_verilog top.v; synth_ecp5 -top top -json artifacts/top.json -abc9"
nextpnr-ecp5 --json artifacts/top.json --textcfg artifacts/top_out.config --45k --package CABGA256 --report artifacts/top.report --freq 50
ecppack artifacts/top_out.config artifacts/top.bit
