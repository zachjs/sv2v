module top;
    reg [31:0] arr;
    wire [7:0] idx;
    assign idx = { 2'b01, 2'b11, 2'b00, 2'b10 };
    initial begin
        arr[idx[0 * 2 +: 2] * 8 +: 8] = 8'hDE;
        arr[idx[1 * 2 +: 2] * 8 +: 8] = 8'hAD;
        arr[idx[2 * 2 +: 2] * 8 +: 8] = 8'hBE;
        arr[idx[3 * 2 +: 2] * 8 +: 8] = 8'hEF;
        $display("%h", arr);
    end
endmodule
