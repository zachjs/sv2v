// pattern: for_loop_init_stray\.sv:3:22: Parse error: expected ',' or ';'
module top;
    initial for (a++ b++; 1; a++) ;
endmodule
