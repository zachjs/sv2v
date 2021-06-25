`define DUMP \
    begin \
        a = 1'sb1; \
        b = 1'sb1; \
        c = 1'sb1; \
        d = 1'sb1; \
        e = 1'sb1; \
        $display("%b %b %b %b %b", a, b, c, d, e); \
    end

module top;
    reg [0:0] a;
    reg [1:0] b;
    reg [63:0] c;
    reg [1:0] d;
    reg [31:0] e;
    initial `DUMP
    generate
        if (1) begin : blk
            reg [0:0] a;
            reg [2:0] b;
            reg [23:0] c;
            reg [2:0] d;
            reg [7:0] e;
            initial
                repeat (4)
                    `DUMP
        end
    endgenerate
endmodule
