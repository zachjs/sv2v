module top;
    generate
        if (1) begin : intf
            wire x;
        end
    endgenerate
    wire y;
    genvar i, j, k;
    generate
        for (i = 2; i >= 0; i = i - 1) begin
            initial $display("ModuleA %b %b", intf.x, y);
            for (j = 2; j >= 0; j = j - 1) begin
                initial $display("ModuleB %b %b", intf.x, y);
                for (k = 2; k >= 0; k = k - 1) begin
                    initial $display("ModuleC %b %b", intf.x, y);
                end
            end
        end
    endgenerate
endmodule
