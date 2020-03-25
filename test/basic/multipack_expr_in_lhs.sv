module top;
    logic [3:0][7:0] arr;
    logic [3:0][1:0] idx;
    assign idx = { 2'b01, 2'b11, 2'b00, 2'b10 };
    initial begin
        arr[idx[0]] = 8'hDE;
        arr[idx[1]] = 8'hAD;
        arr[idx[2]] = 8'hBE;
        arr[idx[3]] = 8'hEF;
        $display("%h", arr);
    end
endmodule
