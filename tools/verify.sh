#!/bin/sh

set -e

mkdir -p "artifacts/"
yosys -p "read_verilog top.v; synth_ecp5 -top top -json artifacts/top.json -abc9"
nextpnr-ecp5 --json artifacts/top.json --textcfg artifacts/top_out.config --um-85k --package CABGA381 --report artifacts/top.report --freq 50 --speed 8
ecppack artifacts/top_out.config artifacts/top.bit
