module top;
    logic [3:0] arr;
    initial
        for (int unsigned i = 0; i < 4; i++)
            arr[i] = i;
    initial $display(arr);

    parameter unsigned foo = 1;
    localparam unsigned bar = 1;
    initial $display(foo, bar);
endmodule
