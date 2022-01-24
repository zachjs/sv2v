module top;
    function automatic bit foo;
        input integer x;
        // no return
    endfunction
    bit x;
    assign x = foo(0);
endmodule
