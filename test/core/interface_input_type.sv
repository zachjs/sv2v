module top;
    intf i();
    mod m(i, 1'b1);
    initial #1 $display("%b", i.y);
endmodule
module mod(
    input intf i,
    input signed x
);
    initial i.y = x;
endmodule
interface intf;
    logic [1:0] y;
endinterface
