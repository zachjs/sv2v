interface Interface;
    logic [0:1][0:2] arr;
endinterface

module Module(intf);
    Interface intf;
endmodule

module top;
    Interface intf();
    Module mod [1][2] (intf);
    assign intf.arr[1] = 6;
    assign intf.arr[0][0] = 1;
    initial $display("%b", intf.arr);
endmodule
