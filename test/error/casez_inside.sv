// pattern: cannot use inside with casez
module top;
    initial
        casez (0) inside
            0: $display("FOO");
            1: $display("BAR");
        endcase
endmodule
