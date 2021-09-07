module mod(a, b, c, d);
    input wire a;
    output reg b;
    always @*
        b = ~a;
    input wire [7:0] c;
    output wire [31:0] d;
    assign d = 10 + c;
endmodule
