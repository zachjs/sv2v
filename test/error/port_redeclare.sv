// pattern: declarations `output logic a` and `logic a` are incompatible due to redeclaration
module top(a);
    output logic a;
    logic a;
endmodule
