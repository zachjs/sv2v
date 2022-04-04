#!/bin/bash
py=$(which python3 || which python)
$py gen.py 4 b 2 01xz > binary.sv
$py gen.py 2 o 8 01234567xz > octal.sv
$py gen.py 2 d 10 0123456789 > decimal.sv
$py gen.py 2 h 16 0123456789abcdefxz > hex.sv
source ../lib/runner.sh
