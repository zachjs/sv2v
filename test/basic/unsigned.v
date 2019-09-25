module top;
    reg [3:0] arr;
    always @* begin : block_name
        integer i;
        for (i = 0; i < 4; i++)
            arr[i] = i;
    end
    initial $display(arr);
endmodule
