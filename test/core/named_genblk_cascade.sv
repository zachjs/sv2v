module mod(
    output wire [31:0] out
);
    parameter P = 0;
    if (P == 1) begin : blk1
        wire w [2];
    end
    else if (P == 2) begin : blk2
        wire x [3];
    end
    else if (P == 3) begin : blk3
        wire y [5];
    end
    else begin : blk4
        wire z [7];
    end
    if (P == 1)
        assign out = $bits(blk1.w);
    else if (P == 2)
        assign out = $bits(blk2.x);
    else if (P == 3)
        assign out = $bits(blk3.y);
    else
        assign out = $bits(blk4.z);
endmodule
