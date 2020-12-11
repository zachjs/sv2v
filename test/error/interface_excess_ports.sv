interface Interface(input a, output b);
    assign b = a;
endinterface

module top;
    logic a, b, c;
    Interface intf(a, b, c);
endmodule
