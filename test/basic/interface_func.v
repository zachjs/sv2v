module top;
    function bar;
        input integer x;
        bar = x * x;
    endfunction
    initial $display(bar(3));
endmodule
