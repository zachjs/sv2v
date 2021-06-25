module top;
    function automatic [31:0] nop;
        input foo;
        nop = 32'h00000013;
    endfunction
    initial $display(nop(0));
endmodule
