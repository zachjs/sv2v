// pattern: for_loop_init_delay\.sv:4:20: Parse error: unexpected timing modifier in for loop initialization
module top;
    integer x;
    initial for (x = #1 1; 1; x++) ;
endmodule
