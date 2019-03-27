`default_nettype none

module top;

    reg [1:0] index;
    reg [7:0] element;
    wire [31:0] arrayData;
    reg enable, clock, clear;
    Array #(.ELEMENTS(4), .WIDTH(8)) arrayInstance(
        .index(index),
        .element(element),
        .array(arrayData),
        .clock(clock),
        .clear(clear),
        .enable(enable)
    );

    wire [7:0] result;
    ArrayOrReduction #(.SIZE(4), .WIDTH(8)) reduction(
        .inputs(arrayData),
        .result(result)
    );

    initial begin
        clock = 1;
        forever #5 clock = ~clock;
    end

    initial begin
        $monitor($time, " arrayData: %h result: %h", arrayData, result);
        clear = 1'b1;
        index = 2'b0;
        element = 8'h0;
        enable = 1'b0;
        repeat(3) @(posedge clock);
        clear = 1'b0;
        element = 8'haa;
        enable = 1'b1;
        @(posedge clock);
        element = 8'h11;
        index = 2'd1;
        @(posedge clock);
        element = 8'h72;
        index = 2'd2;
        @(posedge clock);
        element = 8'h88;
        index = 3'd3;
        @(posedge clock);
        element = 8'hff;
        index = 3'd0;
        enable = 1'b0;
        @(posedge clock);
        element = 8'h00;
        enable = 1'b1;
        @(posedge clock);
        element = 8'h00;
        index = 3'd1;
        @(posedge clock);
        enable = 1'b0;
        index = 3'd2;
        @(posedge clock);
        enable = 1'b1;
        index = 3'd3;
        @(posedge clock);
        enable = 1'b0;
        repeat(5) @(posedge clock);
        $finish;
    end

endmodule
