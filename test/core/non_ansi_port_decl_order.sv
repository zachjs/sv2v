module mod(a, b, c, d);
    input a;
    wire a;
    output b;
    reg b;
    always @*
        b = ~a;
    input [7:0] c;
    wire integer d = 10 + c;
    logic [7:0] c;
    output d;
endmodule
