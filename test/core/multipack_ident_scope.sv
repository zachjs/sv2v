`define DUMP(id) #1 $display(`"id: %b`", arr[0]);
module top;
    parameter A = 5;
    reg [0:0][A-1:0] arr;
    initial arr = 1'sb1;
    initial `DUMP(0)
    if (1) begin : blk
        localparam A = 10;
        initial `DUMP(1)
    end
    initial begin
        localparam A = 10;
        `DUMP(2)
    end
endmodule
