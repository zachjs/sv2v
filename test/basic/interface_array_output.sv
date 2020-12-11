interface Interface(output out);
    assign out = 1;
endinterface

module top;
    logic x;
    Interface intfs[1](x);
    initial $display(x);
endmodule
