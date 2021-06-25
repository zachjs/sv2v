module example(sel, out);
    parameter W = 8;

    reg [7:0] arr [3:0];
    initial begin
        arr[0] = 8'b01000011;
        arr[1] = 8'b00010110;
        arr[2] = 8'b10001111;
        arr[3] = 8'b01100110;
    end

    input [1:0] sel;
    output [7:0] out;

    assign out = sel ? arr[sel] : 8'b0;
endmodule
