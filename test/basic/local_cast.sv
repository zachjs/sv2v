module top;
    generate
        for (genvar i = 1; i < 5; ++i) begin
            initial begin
                integer x, y;
                x = $unsigned(i'(1'sb1));
                y = $unsigned((i + 5)'(1'sb1));
                $display("%0d %b %b", i, x, y);
            end
            for (genvar j = 3; j < 6; ++j) begin
                initial begin
                    integer x;
                    x = $unsigned((i * j)'(1'sb1));
                    $display("%0d %0d %b", i, j, x);
                end
            end
        end
        // TODO: This is not yet supported by iverilog
        // localparam P = 2;
        // for (genvar i = 0; i < int'(P); i = i + 1) begin : g
        //     wire a = i;
        // end
        // initial $display("%b %b", g[0].a, g[1].a);
    endgenerate
endmodule
