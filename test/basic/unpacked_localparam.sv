module top;
    localparam logic [7:0] init_val [4] = {8'd0, 8'd8, 8'd10, 8'd200};
    initial begin
        integer i, j;
        for (i = 0; i < 4; i += 1) begin
            $display(init_val[i]);
            for (j = 0; j < 8; j += 1) begin
                $display(init_val[i][j]);
            end
        end
    end
endmodule
