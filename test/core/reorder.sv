module top;
    assign arr[0][0] = 1;
    logic [1:0][2:0] arr;
    initial $display("%b", arr);
    parameter YES = 1;
    if (YES) begin : blk
        assign brr[0][0] = 1;
        logic [2:0][3:0] brr;
        initial $display("%b", brr);
        if (YES) begin : blk2
            assign crr[0][0] = 1;
            logic [3:0][4:0] crr;
            initial $display("%b", crr);
        end
    end
endmodule
