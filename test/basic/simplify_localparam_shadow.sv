module top;
    localparam i = 1234;
    if (1) begin
        genvar j;
        for (j = 0; j < 10; j = j + 1) begin
            localparam i = j;
            initial $display(i > 5 ? i + 100 : i - 100);
        end
    end
endmodule
