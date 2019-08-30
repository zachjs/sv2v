module top;
    localparam [31:0] init_val = {8'd0, 8'd8, 8'd10, 8'd200};
    initial begin : foo
        integer i, j;
        for (i = 0; i < 4; i += 1) begin
            $display(init_val[8*i+:8]);
            for (j = 0; j < 8; j += 1) begin
                $display(init_val[8*i+j]);
            end
        end
    end
endmodule
