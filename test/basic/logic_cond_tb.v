module top;
    reg inp;

    wire out1;
    wire out2;
    wire out3;

    Example      m1(inp, out1);
    Example #(0) m2(inp, out2);
    Example #(1) m3(inp, out3);

    task dump;
        $display("%b %b %b %b", inp, out1, out2, out3);
    endtask

    initial begin
        #1 dump();
        inp = 0;
        #1 dump();
        inp = 1;
        #1 dump();
        inp = 1'bx;
        #1 dump();
        inp = 1'bz;
        #1 dump();
    end
endmodule
