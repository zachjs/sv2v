module top;
    assign arr[0][0] = 1;
    logic [1:0][2:0] arr;
    initial $display("%b", arr);
endmodule
