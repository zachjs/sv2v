module top;
    generate
        genvar i, j;
        for (i = 1; i < 5; i = i + 1) begin
            initial begin : foo
                integer x, y;
                x = $unsigned(cast_i(1'sb1));
                y = (1 << (i + 5)) - 1;
                $display("%0d %b %b", i, x, y);
            end
            for (j = 3; j < 6; j = j + 1) begin
                initial begin : bar
                    integer x;
                    x = (1 << (i * j)) - 1;
                    $display("%0d %0d %b", i, j, x);
                end
            end
            function signed [i-1:0] cast_i;
                input signed [i-1:0] inp;
                cast_i = inp;
            endfunction
        end
    endgenerate
endmodule
