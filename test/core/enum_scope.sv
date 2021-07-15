module top;
    if (1) begin : blkA
        typedef enum {
            X = 2, Y
        } E;
        initial $display("blkA X=%0d Y=%0d", X, Y);
    end
    if (1) begin : blkB
        typedef enum {
            X, Y
        } E;
        initial $display("blkB X=%0d Y=%0d", X, Y);
        initial begin : blk1
            localparam Y = 100;
            $display("blkB.blk1 X=%0d Y=%0d", X, Y);
        end
        initial begin : blk2
            integer Y;
            Y = 101;
            Y++;
            $display("blkB.blk2 X=%0d Y=%0d", X, Y);
        end
        initial begin : blk3
            localparam type Y = byte;
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
    typedef enum [5:0] {
         X = 22, Y
    } E;
    initial $display("X=%b Y=%b", X, Y);
    if (1) begin : blkC
        typedef byte T;
        localparam A = 1;
        localparam B = 2;
        typedef enum T {
            X = A, Y = B
        } E;
        initial $display("blkC X=%b Y=%b", X, Y);
        initial begin : blk1
            localparam type T = shortint;
            localparam A = 2;
            localparam B = 3;
            $display("blkC.blk1 X=%b Y=%b", X, Y);
        end
    end
endmodule
