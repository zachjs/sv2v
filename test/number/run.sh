#!/bin/bash
python3 gen.py 4 b 2 01xz > binary.sv
python3 gen.py 2 o 8 01234567xz > octal.sv
python3 gen.py 2 d 10 0123456789 > decimal.sv
python3 gen.py 2 h 16 0123456789abcdefxz > hex.sv
source ../lib/runner.sh
