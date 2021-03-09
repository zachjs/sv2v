module top;
    initial
        fork
            $display("A");
            $display("B");
        join
endmodule
