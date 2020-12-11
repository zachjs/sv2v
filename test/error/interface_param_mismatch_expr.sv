interface Interface;
    parameter T = 0;
    logic [T-1:0] x;
endinterface

module top;
    Interface #(logic) intf();
endmodule
