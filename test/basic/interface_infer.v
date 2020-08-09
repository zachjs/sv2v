module top;
    genvar g;
    localparam SOME_VAL = 3;
    generate
        begin : i
            wire x = 0;
            initial $display("Interface %d %d", x, SOME_VAL);
            for (g = 10; g < 15; g = g + 1) begin
                initial $display(g);
            end
        end
    endgenerate
    generate
        begin : m
            initial $display("Module %d", i.x);
            for (g = 0; g < 5; g = g + 1) begin
                initial $display(g);
            end
        end
    endgenerate
endmodule
