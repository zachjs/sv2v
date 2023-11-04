module top;
    function automatic logic [31:0] nop();
        return 32'h00000013;
    endfunction
    initial $display(nop());
    function automatic integer flip;
        input integer inp;
        return ~inp;
    endfunction
    initial $display(flip(nop()));
endmodule
