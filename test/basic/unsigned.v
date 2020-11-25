module top;
    reg [3:0] arr;
    initial begin : block_name
        integer i;
        for (i = 0; i < 4; i++)
            arr[i] = i;
    end
    initial $display(arr);

    parameter foo = 1;
    localparam bar = 1;
    initial $display(foo, bar);
endmodule
