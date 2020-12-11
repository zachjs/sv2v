interface Interface;
    parameter type T;
    T x;
endinterface

module top;
    Interface #(1) intf();
endmodule
