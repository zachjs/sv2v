// pattern: case has multiple defaults
module top;
    case (0)
        0: wire w;
        1: wire x;
        default: wire y;
        default: wire z;
    endcase
endmodule
