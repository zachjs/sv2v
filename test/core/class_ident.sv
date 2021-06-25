class C;
    localparam X = 10;
endclass
module top;
    initial $display(C::X);
endmodule
