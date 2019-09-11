module top;
    typedef struct packed {
        logic f1;
        logic f2;
    } T;

    T a[1:0];
    T b[1:0];

    T c[1:0], d[1:0];
    T e[1:0], f[1:0], g;
    T h, i[1:0];
    T j;

    struct packed {
        logic f1;
        logic f2;
    } k;
endmodule
