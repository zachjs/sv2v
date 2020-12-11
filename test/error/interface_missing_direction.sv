interface Interface(a, b);
    input a;
    logic b;
endinterface

module top;
    logic a, b;
    Interface intf(a, b);
endmodule
