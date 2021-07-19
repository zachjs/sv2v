package P;
    typedef struct packed {
        integer U;
    } T;
    function automatic integer F;
        input integer inp;
        return $clog2(inp);
    endfunction
    localparam X = F(100);
    localparam T Y = '{ U: X + 1 };
    localparam Z = Y.U + 1;
endpackage
import P::*;
localparam V = Y + 1;
module top;
    initial $display("%0d %0d", V, X);
endmodule
