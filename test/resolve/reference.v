module top;
    parameter width = 5;
    input [width-1:0] i;
    output [width-1:0] o;
    assign o = i + 1'b1;
    initial begin
        $display(width);
    end
endmodule
