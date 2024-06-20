`default_nettype none

package P;
    localparam A = 4;
    localparam B = 5;
endpackage

`default_nettype wire

module top;
    if (1) logic [P::A-1:0] w;
    assign x = 0;
    case (1)
    1:
        for (genvar i = 0; i < 1; i++)
            assign y = $bits(genblk1.w);
    endcase
    assign z = y;
endmodule

module extra;
    assign a = 0;
endmodule
