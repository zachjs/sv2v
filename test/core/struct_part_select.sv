module Example;
    typedef struct packed {
        logic [10:4] a;
        logic [1:3] bx;
        logic [3:1] by;
        logic [3:4][5:7] cw;
        logic [4:3][5:7] cx;
        logic [3:4][7:5] cy;
        logic [4:3][7:5] cz;
    } T;
    T t;
    initial begin
        $monitor("%2d %b %b %b %b %b %b %b %b %b %b %b %b %b %b %b %b", $time,
            t, t.a, t.bx, t.by,
            t.cw, t.cw[3], t.cw[4],
            t.cx, t.cx[3], t.cx[4],
            t.cy, t.cy[3], t.cy[4],
            t.cz, t.cz[3], t.cz[4]
            );

        #1 t.a = 1;
        #1 t.a[5+:2] = '1;
        #1 t.a[8-:3] = '1;
        #1 t.a[10] = 1;
        #1 t.a[7] = 0;

        #1 t.bx[1+:1] = 1;
        #1 t.bx[1:2] = 1;
        #1 t.bx[3] = 0;
        #1 t.bx[3-:2] = 1;
        #1 t.bx[2] = 0;

        #1 t.by[1+:1] = 1;
        #1 t.by[2:1] = 1;
        #1 t.by[3] = 0;
        #1 t.by[3-:2] = 1;
        #1 t.by[2] = 0;

        #1 t.cw[3][6+:1] = 1;
        #1 t.cw[3][7-:2] = 1;
        #1 t.cw[3][5+:2] = 0;
        #1 t.cw[3][6:7] = 2'b10;
        #1 t.cw[3][6:7] = 2'b01;
        #1 t.cw[3:4] = '1;
        #1 t.cw[4][5] = 0;
        #1 t.cw[4][6:7] = 0;
        #1 t.cw[3+:2] = 6'b010011;
        #1 t.cw[4-:2] = 6'b101011;

        #1 t.cx[3][6+:1] = 1;
        #1 t.cx[3][7-:2] = 1;
        #1 t.cx[3][5+:2] = 0;
        #1 t.cx[3][6:7] = 2'b10;
        #1 t.cx[3][6:7] = 2'b01;
        #1 t.cx[4:3] = '1;
        #1 t.cx[4][5] = 0;
        #1 t.cx[4][6:7] = 0;
        #1 t.cx[3+:2] = 6'b010011;
        #1 t.cx[4-:2] = 6'b101011;

        #1 t.cy[3][6+:1] = 1;
        #1 t.cy[3][7-:2] = 1;
        #1 t.cy[3][5+:2] = 0;
        #1 t.cy[3][7:6] = 2'b10;
        #1 t.cy[3][7:6] = 2'b01;
        #1 t.cy[3:4] = '1;
        #1 t.cy[4][5] = 0;
        #1 t.cy[4][7:6] = 0;
        #1 t.cy[3+:2] = 6'b010011;
        #1 t.cy[4-:2] = 6'b101011;

        #1 t.cz[3][6+:1] = 1;
        #1 t.cz[3][7-:2] = 1;
        #1 t.cz[3][5+:2] = 0;
        #1 t.cz[3][7:6] = 2'b10;
        #1 t.cz[3][7:6] = 2'b01;
        #1 t.cz[4:3] = '1;
        #1 t.cz[4][5] = 0;
        #1 t.cz[4][7:6] = 0;
        #1 t.cz[3+:2] = 6'b010011;
        #1 t.cz[4-:2] = 6'b101011;

    end
endmodule
module top;
    Example e();
endmodule
