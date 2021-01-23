module top;
    function automatic integer func;
        input integer inp;
        func = inp * 2;
    endfunction
    localparam integer X = func(1);
    initial $display(X);
endmodule
