// pattern: cannot use inside with casex
module top;
    initial
        casex (0) inside
            0: $display("FOO");
            1: $display("BAR");
        endcase
endmodule
