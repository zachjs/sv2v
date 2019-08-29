module top;
    function automatic logic [31:0] nop();
        return 32'h00000013;
    endfunction
    initial $display(nop());
endmodule
