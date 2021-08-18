module mod(input [5:0] x);
    initial #1 $display("%b %b %b", x[4+:2], x[2+:2], x[0+:2]);
endmodule

module top;
    reg [5:0] a;
    wire [5:0] b;
    always @(*) a = b;

    reg x;
    wire [5:0] c;
    wire [5:0] d;
    wire [5:0] e;
    wire [5:0] f;
    initial x = 0;
    assign c = x ? d : !x ? e : f;

    reg [5:0] l;
    reg [5:0] m;
    reg [5:0] n;
    initial begin
        x = 1;
        l = { 2'bXZ, 2'b01, 2'b10 };
        m = { 2'b01, 2'b10, 2'b11 };
        n = { 2'b10, 2'b00, 2'b10 };
    end
    mod mod(!x ? l : x ? m : n);

    generate
        if (1) begin : A
            wire [1:0] c [0:2];
            wire [5:0] d;
        end
    endgenerate
    assign A.d = 0;
    initial $display("%b %b", A.c[0], A.d[1:0]);
endmodule
