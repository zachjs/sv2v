// pattern: missing expected `join`
module top;
    initial
        fork
            $display("FOO");
            $display("BAR");
endmodule
