// pattern: for_loop_init_nblk\.sv:4:20: Parse error: unexpected non-blocking assignment in for loop initialization
module top;
    integer x;
    initial for (x <= 1; 1; x++) ;
endmodule
