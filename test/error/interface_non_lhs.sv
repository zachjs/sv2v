interface Interface(input a, output b);
    assign b = a;
endinterface

module top;
    logic a, b;
    Interface intf(a, b + 1);
endmodule
