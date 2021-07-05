// pattern: for_loop_decl_no_init\.sv:4:14: Parse error: for loop declaration of "x" is missing initialization
module top;
    initial
        for (integer x; x < 3; x = x + 1)
            ;
endmodule
