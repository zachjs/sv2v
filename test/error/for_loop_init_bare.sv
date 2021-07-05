// pattern: for_loop_init_bare\.sv:3:19: Parse error: expected assignment operator
module top;
    initial for (a,; 1; a++) ;
endmodule
