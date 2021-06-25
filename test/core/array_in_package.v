module top;
    localparam [11:0] ARR = 12'h3EA;
    initial begin : foo
        integer i, j;
        for (i = 0; i < 4; ++i)
            for (j = 0; j < 3; ++j)
                $display("ARR[%0d][%0d] = %0d", i, j, ARR[i * 3 + j]);
        for (i = 0; i < 4; ++i)
            for (j = 0; j < 3; ++j)
                $display("ARR[%0d][%0d] = %0d", i, j, ARR[i * 3 + j]);
    end
endmodule
