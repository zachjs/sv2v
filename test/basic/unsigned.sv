module top;
    logic [3:0] arr;
    always_comb
        for (int unsigned i = 0; i < 4; i++)
            arr[i] = i;
    initial $display(arr);
endmodule
