module top;
    logic [2:0][3:0] arr [1:0];
    initial begin
        for (int i = 0; i <= 1; i++) begin
            for (int j = 0; j <= 2; j++) begin
                for (int k = 0; k <= 3; k++) begin
                    $display("%b", arr[i][j][k]);
                    arr[i][j][k] = 1'(i+j+k);
                    $display("%b", arr[i][j][k]);
                end
            end
        end
    end
endmodule
