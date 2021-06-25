module top;
    typedef struct packed {
        integer a, b, c;
    } S;
    S s = '{a: 1, b: 2, c: 3};
    initial #1 $display("%b %b %b %b", s, s.a, s.b, s.c);
endmodule
