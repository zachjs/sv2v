module top;
    typedef struct packed {
        logic a, b;
    } T;
    typedef struct packed {
        logic x;
        T y;
    } S;

    localparam WIDTH = 1;
    S [WIDTH] s = '{
        '{x: 1, y: '{a: 1, b: 0}}
    };

    initial #1 $display("%b", s);
endmodule
