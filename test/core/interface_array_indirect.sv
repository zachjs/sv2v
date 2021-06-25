interface Interface;
    integer x;
endinterface

module Single(intf);
    Interface intf;
    initial #1 $display("Single %0d", intf.x);
endmodule

module Group(intfs);
    parameter WIDTH = 1;
    Interface intfs [WIDTH];
    initial $display("Group %0d", WIDTH);
    for (genvar i = 0; i < WIDTH; ++i)
        Single s(intfs[i]);
endmodule

module top;
    Interface intfs[8]();
    for (genvar i = 0; i < 8; ++i)
        initial intfs[i].x = i ** 3;
    Group #(.WIDTH(8)) g(intfs);
endmodule
