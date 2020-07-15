module top;
    logic [1:0] a [3];
    logic [1:0] b [3];
    always_comb a = b;

    logic x;
    logic [1:0] c [3];
    logic [1:0] d [3];
    logic [1:0] e [3];
    initial x = 0;
    assign c = x ? d : e;

    generate
        begin : A
            logic [1:0] c [3];
            logic [1:0] d [3];
        end
    endgenerate
    assign A.d = 0;
    initial $display("%b %b", A.c[0], A.d[0]);
endmodule
