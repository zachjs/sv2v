module top;
    localparam FMT = "%s=%b %s=%b %s=%b %s=%b %b";
    initial begin
        $display(FMT,
            "A", 8'd0,
            "B", 8'd1,
            "B", 8'd1,
            "C", 8'd2,
            3);
        $display(FMT,
            "D", 16'd0,
            "E", 16'd1,
            "E", 16'd1,
            "F", 16'd2,
            3);
    end
endmodule
