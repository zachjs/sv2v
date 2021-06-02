module top;
    if (1) begin : intf1
        wire [0:1][0:2] arr;
    end
    if (1) begin : intf2
        wire [0:1][0:2] arr;
    end
    assign intf2.arr[1] = 1;
    assign intf2.arr[0][0] = 0;
    initial $display("2: %b", intf2.arr);
    assign intf1.arr[1] = 6;
    assign intf1.arr[0][0] = 1;
    initial $display("1: %b", intf1.arr);
endmodule
