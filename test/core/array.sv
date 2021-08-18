module mod(input [1:0] x [3]);
    initial #1 $display("%b %b %b", x[0], x[1], x[2]);
endmodule

module top;
    logic [1:0] a [3];
    logic [1:0] b [3];
    always_comb a = b;

    logic x;
    logic [1:0] c [3];
    logic [1:0] d [3];
    logic [1:0] e [3];
    logic [1:0] f [3];
    initial x = 0;
    assign c = x ? d : !x ? e : f;

    logic [1:0] l [3];
    logic [1:0] m [3];
    logic [1:0] n [3];
    initial begin
        x = 1;
        {l[0], l[1], l[2]} = { 2'bXZ, 2'b01, 2'b10 };
        {m[0], m[1], m[2]} = { 2'b01, 2'b10, 2'b11 };
        {n[0], n[1], n[2]} = { 2'b10, 2'b00, 2'b10 };
    end
    mod mod(!x ? l : x ? m : n);

    generate
        begin : A
            logic [1:0] c [3];
            logic [1:0] d [3];
        end
    endgenerate
    assign A.d = '{ default: 0 };
    initial $display("%b %b", A.c[0], A.d[0]);
endmodule
