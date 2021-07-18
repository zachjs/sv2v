module top;
    generate
        genvar i, j;
        for (i = 1; i < 5; i = i + 1) begin
            localparam [i - 1:0] A = 1'sb1;
            localparam [i + 4:0] B = 1'sb1;
            initial begin : foo
                integer x, y;
                x = $unsigned(cast_i(1'sb1));
                y = (1 << (i + 5)) - 1;
                $display("%0d %b %b %b %b", i, x, y, A, B);
            end
            for (j = 3; j < 6; j = j + 1) begin
                localparam [i * j - 1:0] C = 1'sb1;
                initial begin : bar
                    integer x;
                    x = (1 << (i * j)) - 1;
                    $display("%0d %0d %b %b", i, j, x, C);
                end
            end
            function signed [i-1:0] cast_i;
                input signed [i-1:0] inp;
                cast_i = inp;
            endfunction
        end
        localparam P = 2;
        for (i = 0; i < P; i = i + 1) begin : g
            wire a = i;
        end
        initial $display("%b %b", g[0].a, g[1].a);
    endgenerate
endmodule
