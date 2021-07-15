module top;
generate
    if (1) begin : blkA
        localparam X = 2;
        localparam Y = X + 1;
        initial $display("blkA X=%0d Y=%0d", X, Y);
    end
    if (1) begin : blkB
        localparam X = 0;
        localparam Y = X + 1;
        initial $display("blkB X=%0d Y=%0d", X, Y);
        initial begin : blk1
            localparam Y = 100;
            $display("blkB.blk1 X=%0d Y=%0d", X, Y);
        end
        initial begin : blk2
            integer Y;
            Y = 102;
            $display("blkB.blk2 X=%0d Y=%0d", X, Y);
        end
        initial begin : blk3
            reg [7:0] Y;
            $display("blkB.blk3 $bits(X)=%0d $bits(Y)=%0d", $bits(X), $bits(Y));
        end
        if (1) begin : blk4
            genvar X;
            for (X = 0; X < 2; X = X + 1)
                initial $display("blkB.blk4 X=%0d Y=%0d", X, Y);
        end
        if (1) begin : blk5
            wire [3:0] X = 7;
            initial $display("blkB.blk5 X=%0d Y=%0d", X, Y);
        end
    end
    localparam [5:0] X = 22;
    localparam [5:0] Y = X + 1;
    initial $display("X=%b Y=%b", X, Y);
    if (1) begin : blkC
        localparam signed [7:0] X = 1;
        localparam signed [7:0] Y = 2;
        initial $display("blkC X=%b Y=%b", X, Y);
        initial begin : blk1
            $display("blkC.blk1 X=%b Y=%b", X, Y);
        end
    end
endgenerate
endmodule
