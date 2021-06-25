module top;
    reg arr [1:0][2:0][3:0];
    initial begin : block_name
        integer i, j, k;
        for (i = 0; i <= 1; i++) begin
            for (j = 0; j <= 2; j++) begin
                for (k = 0; k <= 3; k++) begin
                    $display("%b", arr[i][j][k]);
                    arr[i][j][k] = i+j+k;
                    $display("%b", arr[i][j][k]);
                end
            end
        end
    end
endmodule
