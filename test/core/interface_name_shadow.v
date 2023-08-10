module A(input wire [3:0] i);
    initial $display("A %b", i);
endmodule
module B(input wire [3:0] i);
    initial $display("B %b", i);
endmodule
module top;
    generate
        if (1) begin : i
            wire [3:0] x;
        end
        if (1) begin : blk
            wire i;
            assign i = 0;
        end
    endgenerate
    initial $display("%b %b", i.x, blk.i);
    A a(i.x);
    B b(i.x);
    assign i.x = 1;
endmodule
