package P;
    localparam A = 4;
endpackage
module top;
    generate
        if (1) generate
            logic [P::A - 1:0] w;
        endgenerate
    endgenerate
    generate
        case (1)
            1: generate
                genvar i;
                for (i = 0; i < 1; i += 1) generate
                    assign y = $bits(genblk1.w);
                endgenerate
            endgenerate
        endcase
    endgenerate
    assign z = y;
endmodule
