module Suite;
    parameter WIDTH = 1;
    `include "inside_exhaust.vh"

    function [0:0] test_weq;
        input [WIDTH-1:0] x, y;
        return x ==? y;
    endfunction

    function [0:0] test_wne;
        input [WIDTH-1:0] x, y;
        return x !=? y;
    endfunction

    function [0:0] test_inside;
        input [WIDTH-1:0] x, y, z;
        return x inside {y, z};
    endfunction
endmodule

module top;
    Suite #(1) a();
    Suite #(2) b();
    Suite #(3) c();
endmodule
