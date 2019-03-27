`default_nettype none

module top;

    reg [2:0] operation;
    reg [31:0] left, right;
    wire [31:0] result;

    ALU dut(
        .operation(operation),
        .left(left),
        .right(right),
        .result(result)
    );

    initial begin
        $monitor($time, " %h %d %h = %h", left, operation, right, result);
        operation = 3'd0;
        left = 32'h0;
        right = 32'h0;
        #10;
        left = 32'h80000000;
        right = 32'd16;
        #10;
        operation = 3'd1;
        #10;
        left = 32'h7fff0003;
        right = 32'd1;
        #10;
        right = 32'd0;
        #10;
        right = 32'd8;
        #10;
        operation = 3'd2;
        left = 32'hffffffff;
        right = 32'h10;
        #10;
        left = 32'h1;
        right = 32'h10;
        #10;
        left = 32'h10;
        right = 32'hffffffff;
        #10;
        operation = 3'd3;
        left = 32'h80000000;
        right = 32'h7fffffff;
        #10;
        left = 32'hff;
        right = 32'h80000000;
        #10;
        operation = 3'd2;
        left = 32'd10;
        right = 32'd20;
        #10;
        left = 32'd20;
        right = 32'd10;
        #10;
        left = 32'hffffffff; // -1
        right = 32'hfffff000;
        #10;
        left = 32'hfffff000;
        right = 32'hffffffff;
        #10;
        left = 32'hfffff000;
        right = 32'h10;
        #10;
        left = 32'h10;
        right = 32'hfffff000;
        #10;
        // operation = 3'd1;
        // // for(left = 32'b0; left < 32'hffffffff; left = left + 32'b1)
        // //     for(right = 32'b0; right <= 32'd31; right = right + 32'b1)
        // //         #10 if(result != dut.result2) $error("Bad match: %h %h", result, dut.result2);
        // // #10;
        // left = 32'hffffffff;
        // for(right = 32'b0; right <= 32'd31; right = right + 32'd1)
        //     #10 if(result != dut.result2) $error("Bad match: %h %h", result, dut.result2);
        // #10;
        $finish;
    end

endmodule
