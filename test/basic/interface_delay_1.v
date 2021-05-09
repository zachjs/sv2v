module top;
    if (1) begin : intf
        wire [0:1][0:2] arr;
    end
    assign intf.arr[1] = 6;
    assign intf.arr[0][0] = 1;
    initial $display("%b", intf.arr);
endmodule
