interface Interface;
    parameter type T = logic [2:0];
    initial $display($bits(T));
endinterface

module top;
    Interface #(logic) a();
    Interface #(byte) b();
    Interface c();
endmodule
