module top;
    wire [5:0] arr;
    assign arr[0] = 1;
    initial $display("%b", arr);
endmodule
