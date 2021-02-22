`define DUMP(id) #1 $display(`"id: %b`", arr);
module top;
    parameter A = 5;
    reg [A-1:0] arr;
    initial arr = 1'sb1;
    initial `DUMP(0)
    if (1) begin : blk
        localparam A = 10;
        initial `DUMP(1)
    end
    initial begin : foo
        localparam A = 10;
        `DUMP(2)
    end
endmodule
