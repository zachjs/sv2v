module top;
    typedef struct packed {
        logic [2][3] x;
    } S;
    S s;
    initial s = S'('1);
endmodule
