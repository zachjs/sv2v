// pattern: case has multiple defaults
module top;
    initial
        case (0)
            0: $display("FOO");
            1: $display("BAR");
            default: $display("A");
            default: $display("B");
        endcase
endmodule
