module top;
    generate
        for (genvar i = 1; i < 5; ++i) begin
            localparam A = $unsigned(i'(1'sb1));
            localparam B = $unsigned((i + 5)'(1'sb1));
            initial begin
                integer x, y;
                x = $unsigned(i'(1'sb1));
                y = $unsigned((i + 5)'(1'sb1));
                $display("%0d %b %b %b %b", i, x, y, A, B);
            end
            for (genvar j = 3; j < 6; ++j) begin
                localparam C = $unsigned((i * j)'(1'sb1));
                initial begin
                    integer x;
                    x = $unsigned((i * j)'(1'sb1));
                    $display("%0d %0d %b %b", i, j, x, C);
                end
            end
        end
        localparam P = 2;
        for (genvar i = 0; i < byte'(P); i = i + 1) begin : g
            wire a = i;
        end
        initial $display("%b %b", g[0].a, g[1].a);
    endgenerate
endmodule
