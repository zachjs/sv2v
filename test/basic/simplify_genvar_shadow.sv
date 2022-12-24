module top;
    localparam i = 1234;
    if (1) begin
        genvar i;
        for (i = 0; i < 10; i = i + 1)
            initial $display(i > 5 ? i + 100 : i - 100);
    end
endmodule
