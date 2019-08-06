`define FANCY_SEEING_YOU 1337
module top;
    localparam width = 5;
    input [width-1:0] i;
    output [width-1:0] o;
    assign o = i + 1'b1;
    initial begin
        $display(width);
        $display(`FANCY_SEEING_YOU);
    end
endmodule
