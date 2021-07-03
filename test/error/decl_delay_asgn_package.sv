// pattern: decl_delay_asgn_package\.sv:3:13: Parse error: unexpected timing modifier in declaration
package P;
    logic x = #1 1;
endpackage
module top;
    import P::*;
endmodule
