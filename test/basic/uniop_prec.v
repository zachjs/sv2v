module Example(a, b);
    input wire [1:0] a;
    output wire b;
    assign b = !(&a);
endmodule
