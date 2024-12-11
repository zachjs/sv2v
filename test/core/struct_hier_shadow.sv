package P;
    localparam [31:0] L = 8;
    function automatic integer F;
        F = -1;
    endfunction
    typedef struct packed {
        logic [L + L[0] + L[1:0] + L[0+:1] + F():0] x;
    } S;
endpackage
module top;
    P::S top;
    logic [2:0] x;
    assign x = '0;
    assign top.x = '1;
    initial begin
        #1;
        $display("%b %b", x, top.x);
        $display("%b %b", x[0], top.x[0]);
        $display("%b %b", x[1:0], top.x[1:0]);
        $display("%b %b", x[0+:1], top.x[0+:1]);
    end
    parameter int Q = 2;
    typedef struct {
        logic [Q-1:0] y;
    } T;
    T y;
    assign y.y = 0;
endmodule
