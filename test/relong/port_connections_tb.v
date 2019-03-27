`default_nettype none

module top;

    reg [31:0] data;
    wire parity;

    Device dut(
        .data(data),
        .parity(parity)
    );

    initial begin
        $monitor($time, " data: %h parity: %b", data, parity);
        data = 32'b0;
        #10 data = 32'h00000003;
        #10 data = 32'h00000300;
        #10 data = 32'h00030000;
        #10 data = 32'h03000000;
        #10 data = 32'h01010101;
        #10 data = 32'h01000101;
        #10 data = 32'h00010101;
        #10 data = 32'h01010001;
        #10 data = 32'h01010100;
        #10 data = 32'hffffffff;
        #10 data = 32'hfeffffff;
        #10 $finish;
    end

endmodule
