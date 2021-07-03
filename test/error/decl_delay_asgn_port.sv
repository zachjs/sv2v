// pattern: decl_delay_asgn_port\.sv:3:14: Parse error: unexpected timing modifier in declaration
module top(
    output x = #1 1
);
endmodule
