// pattern: declarations `output a = 1` and `logic a` are incompatible due to invalid initialization at port declaration
module top(a);
    output a = 1;
    logic a;
endmodule
