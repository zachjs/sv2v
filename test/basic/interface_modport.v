module Tester(input clock);
    parameter WIDTH = 1;

    localparam DATA_WIDTH = 2 ** WIDTH;

    reg [2*DATA_WIDTH-1:0] x;
    initial x = 1;

    wire [WIDTH-1:0] idx1, idx2;
    assign idx1 = $clog2(x[2*DATA_WIDTH-1:DATA_WIDTH]);
    assign idx2 = $clog2(x[DATA_WIDTH-1:0]);

    integer i = 0;
    initial #1 $display("shadow i = %d, %b", i, x);

    always @(posedge clock) begin : block
        localparam SIZE = 2 * DATA_WIDTH;
        integer i;
        reg temp;
        temp = x[SIZE-1];
        for (i = SIZE-1; i > 0; i = i - 1) begin
            x[i] = x[i-1];
        end
        x[0] = temp;
    end

    always @(negedge clock)
        $display("%d %0d %2d %2d %b", $time, WIDTH, idx1, idx2, x);
endmodule
