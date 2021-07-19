module top;
    typedef struct packed {
        integer x;
        byte y;
    } S;
    typedef struct packed {
        byte x;
        shortint y;
        S z;
    } T;
    initial $display("%b", T'{ x: 1, y: 2, z: '{ x: 3, y: 4 } });
endmodule
