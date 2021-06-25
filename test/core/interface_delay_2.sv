interface Interface;
    logic [0:1][0:2] arr;
endinterface

module Module(intf);
    Interface intf [3:3][1:2];
    assign intf[3][2].arr[1] = 1;
    assign intf[3][2].arr[0][0] = 0;
    initial $display("2: %b", intf[3][2].arr);
endmodule

module top;
    Interface intf [1:1][1:2] ();
    Module mod (intf);
    assign intf[1][1].arr[1] = 6;
    assign intf[1][1].arr[0][0] = 1;
    initial $display("1: %b", intf[1][1].arr);
endmodule
