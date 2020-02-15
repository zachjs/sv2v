module Example(a, b);
    input logic [1:0] a;
    output logic b;
    assign b = !(&a);
endmodule
