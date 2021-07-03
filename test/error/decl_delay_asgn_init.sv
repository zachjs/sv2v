// pattern: decl_delay_asgn_init\.sv:4:24: Parse error: unexpected timing modifier in declaration
module top;
    initial
        for (integer x = #1 1; 1; x++)
            $display("Hi!");
endmodule
