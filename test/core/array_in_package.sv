package pkg;
    localparam ARR [3:0][2:0] = 12'h3EA;
endpackage

module top;
    import pkg::*;
    initial begin
        for (integer i = 0; i < 4; ++i)
            for (integer j = 0; j < 3; ++j)
                $display("ARR[%0d][%0d] = %0d", i, j, ARR[i][j]);
        for (integer i = 0; i < 4; ++i)
            for (integer j = 0; j < 3; ++j)
                $display("ARR[%0d][%0d] = %0d", i, j, pkg::ARR[i][j]);
    end
endmodule
