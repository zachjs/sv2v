module Module(x, y);
    inout x, y;
    parameter DIR = 1;
    if (DIR)
        assign x = y;
    else
        assign y = x;
endmodule

module top;
    wire inp = 1;
    wire out1, out2;
    Module #(0) fwd(inp, out1);
    Module #(1) rev(out2, inp);
    initial $display("%b %b %b", inp, out1, out2);
endmodule
