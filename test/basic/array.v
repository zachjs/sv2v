module top;
    reg [5:0] a;
    wire [5:0] b;
    always @(*) a = b;

    reg x;
    wire [5:0] c;
    wire [5:0] d;
    wire [5:0] e;
    initial x = 0;
    assign c = x ? d : e;

    generate
        begin : A
            wire [1:0] c [0:2];
            wire [5:0] d;
        end
    endgenerate
    assign A.d = 0;
    initial $display("%b %b", A.c[0], A.d[1:0]);
endmodule
