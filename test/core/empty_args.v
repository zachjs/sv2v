module top;
    function automatic [31:0] nop;
        input foo;
        nop = 32'h00000013;
    endfunction
    initial $display(nop(0));
    function automatic integer flip;
        input integer inp;
        flip = ~inp;
    endfunction
    initial $display(flip(nop(0)));
endmodule
