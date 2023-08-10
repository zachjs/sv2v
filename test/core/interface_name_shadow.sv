interface I;
    logic [3:0] x;
endinterface
module A(I i);
    initial $display("A %b", i.x);
endmodule
module B #(localparam type I = logic [3:0]) (I i);
    initial $display("B %b", i);
endmodule
module top;
    I i();
    if (1) begin : blk
        typedef logic I;
        var I i;
        assign i = 0;
    end
    initial $display("%b %b", i.x, blk.i);
    A a(i);
    B b(i.x);
    assign i.x = 1;
endmodule
