typedef logic over;

interface intf;
    logic [3:0] x;
    assign x[0] = 0;
    initial $display("intf x %b", x);
endinterface

module mod(intf i);
    assign i.x[1] = 1;
    initial $display("mod i.x %b", i.x);
endmodule

module check;
    over y;
    intf over();
    mod m(over);
    assign over.x[2] = 1'bz;
    initial $display("check over.x %b", over.x);
    initial $display("check y %b", y);
endmodule

module top;
    check c();
    intf over();
    mod m(over);
    assign over.x[2] = 1'bz;
    initial $display("top over.x %b", over.x);
endmodule
