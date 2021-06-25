module top;
    wire [5:0] arr;
    assign arr[0] = 1;
    initial $display("%b", arr);
    parameter YES = 1;
    generate
        if (YES) begin : blk
            wire [11:0] brr;
            assign brr[0] = 1;
            initial $display("%b", brr);
            if (YES) begin : blk2
                assign crr[0] = 1;
                wire [19:0] crr;
                initial $display("%b", crr);
            end
        end
    endgenerate
endmodule
