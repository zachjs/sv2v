module Suite;
    parameter WIDTH = 1;
    `include "inside_exhaust.vh"

    function [0:0] test_weq;
        input [WIDTH-1:0] x, y;
        integer idx;
        begin
            test_weq = 1'b1;
            for (idx = 0; idx < WIDTH; idx = idx + 1) begin
                if (y[idx] === 1'bx || y[idx] === 1'bz)
                    ;
                else if (x[idx] === 1'bx || x[idx] === 1'bz)
                    test_weq = 1'bx;
                else if (y[idx] !== x[idx]) begin
                    test_weq = 1'b0;
                    idx = WIDTH;
                end
            end
        end
    endfunction

    function [0:0] test_wne;
        input [WIDTH-1:0] x, y;
        test_wne = !test_weq(x, y);
    endfunction

    function [0:0] test_inside;
        input [WIDTH-1:0] x, y, z;
        test_inside = |{test_weq(x, y), test_weq(x, z)};
    endfunction
endmodule

module top;
    Suite #(1) a();
    Suite #(2) b();
    Suite #(3) c();
endmodule
