// pattern: decl_delay_asgn\.sv:3:13: Parse error: unexpected timing modifier in declaration
module top;
    logic x = #1 1;
endmodule
