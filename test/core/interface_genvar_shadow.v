module top;
    genvar i, j, k;
    generate
        for (i = 1; i <= 8; i = i * 2) begin : blk
            for (j = 0; j < 4; j = j + 1) begin : blk
                wire [i - 1:0] x;
                assign x = i;
                initial $display("intf %b", x);
            end
            for (j = 0; j < 4; j = j + 1) begin : alt
                for (k = 0; k < 2; k = k + 1)
                    initial $display("foo %0d", k);
                initial $display("mod [%0d] %b", j, top.blk[i].blk[j].x);
                initial $display("mod shadowed_intf %0d %b", j, top.blk[i].blk[j].x);
            end
            for (k = 0; k < 2; k = k + 1)
                initial $display("bar %0d", k);
        end
    endgenerate
endmodule
