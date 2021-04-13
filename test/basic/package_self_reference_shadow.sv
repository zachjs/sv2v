package P;
    localparam Bar = 1;
    function automatic integer func;
        localparam Bar = 2;
        func = Bar + P::Bar;
    endfunction
endpackage
module top;
    import P::*;
    initial $display(func());
endmodule
