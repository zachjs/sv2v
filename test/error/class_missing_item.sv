// pattern: could not find "Y" in class "C"
class C;
    localparam X = 10;
endclass
module top;
    initial $display(C::Y);
endmodule
