module top;
    generate
        if (1) begin : block
            wire [3:2][8:5] xs;
            genvar x, y;
            for (x = 2; x <= 3; x = x + 1)
                for (y = 5; y <= 8; y = y + 1)
                    assign xs[x][y] = 1'b1;
        end
    endgenerate
endmodule
