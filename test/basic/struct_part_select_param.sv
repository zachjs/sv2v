module Example;
    // The test is equivalent to the struct_part_select test, except that all of
    // the dimensions and indices have been replaced with parameters. This
    // ensures sv2v can generate reasonable output even when nothing can be
    // simplified down at the time of conversion.
    parameter ONE = 1;
    parameter TWO = 2;
    parameter THREE = 3;
    parameter FOUR = 4;
    parameter FIVE = 5;
    parameter SIX = 6;
    parameter SEVEN = 7;
    parameter EIGHT = 8;
    parameter NINE = 9;
    parameter TEN = 10;

    typedef struct packed {
        logic [TEN:FOUR] a;
        logic [ONE:THREE] bx;
        logic [THREE:ONE] by;
        logic [THREE:FOUR][FIVE:SEVEN] cw;
        logic [FOUR:THREE][FIVE:SEVEN] cx;
        logic [THREE:FOUR][SEVEN:FIVE] cy;
        logic [FOUR:THREE][SEVEN:FIVE] cz;
    } T;
    T t;
    initial begin
        $monitor("%2d %b %b %b %b %b %b %b %b %b %b %b %b %b %b %b %b", $time,
            t, t.a, t.bx, t.by,
            t.cw, t.cw[THREE], t.cw[FOUR],
            t.cx, t.cx[THREE], t.cx[FOUR],
            t.cy, t.cy[THREE], t.cy[FOUR],
            t.cz, t.cz[THREE], t.cz[FOUR]
            );

        #1 t.a = 1;
        #1 t.a[FIVE+:TWO] = '1;
        #1 t.a[EIGHT-:THREE] = '1;
        #1 t.a[TEN] = 1;
        #1 t.a[SEVEN] = 0;

        #1 t.bx[ONE+:ONE] = 1;
        #1 t.bx[ONE:TWO] = 1;
        #1 t.bx[THREE] = 0;
        #1 t.bx[THREE-:TWO] = 1;
        #1 t.bx[TWO] = 0;

        #1 t.by[ONE+:ONE] = 1;
        #1 t.by[TWO:ONE] = 1;
        #1 t.by[THREE] = 0;
        #1 t.by[THREE-:TWO] = 1;
        #1 t.by[TWO] = 0;

        #1 t.cw[THREE][SIX+:ONE] = 1;
        #1 t.cw[THREE][SEVEN-:TWO] = 1;
        #1 t.cw[THREE][FIVE+:TWO] = 0;
        #1 t.cw[THREE][SIX:SEVEN] = 2'b10;
        #1 t.cw[THREE][SIX:SEVEN] = 2'b01;
        #1 t.cw[THREE:FOUR] = '1;
        #1 t.cw[FOUR][FIVE] = 0;
        #1 t.cw[FOUR][SIX:SEVEN] = 0;
        #1 t.cw[THREE+:TWO] = 6'b010011;
        #1 t.cw[FOUR-:TWO] = 6'b101011;

        #1 t.cx[THREE][SIX+:ONE] = 1;
        #1 t.cx[THREE][SEVEN-:TWO] = 1;
        #1 t.cx[THREE][FIVE+:TWO] = 0;
        #1 t.cx[THREE][SIX:SEVEN] = 2'b10;
        #1 t.cx[THREE][SIX:SEVEN] = 2'b01;
        #1 t.cx[FOUR:THREE] = '1;
        #1 t.cx[FOUR][FIVE] = 0;
        #1 t.cx[FOUR][SIX:SEVEN] = 0;
        #1 t.cx[THREE+:TWO] = 6'b010011;
        #1 t.cx[FOUR-:TWO] = 6'b101011;

        #1 t.cy[THREE][SIX+:ONE] = 1;
        #1 t.cy[THREE][SEVEN-:TWO] = 1;
        #1 t.cy[THREE][FIVE+:TWO] = 0;
        #1 t.cy[THREE][SEVEN:SIX] = 2'b10;
        #1 t.cy[THREE][SEVEN:SIX] = 2'b01;
        #1 t.cy[THREE:FOUR] = '1;
        #1 t.cy[FOUR][FIVE] = 0;
        #1 t.cy[FOUR][SEVEN:SIX] = 0;
        #1 t.cy[THREE+:TWO] = 6'b010011;
        #1 t.cy[FOUR-:TWO] = 6'b101011;

        #1 t.cz[THREE][SIX+:ONE] = 1;
        #1 t.cz[THREE][SEVEN-:TWO] = 1;
        #1 t.cz[THREE][FIVE+:TWO] = 0;
        #1 t.cz[THREE][SEVEN:SIX] = 2'b10;
        #1 t.cz[THREE][SEVEN:SIX] = 2'b01;
        #1 t.cz[FOUR:THREE] = '1;
        #1 t.cz[FOUR][FIVE] = 0;
        #1 t.cz[FOUR][SEVEN:SIX] = 0;
        #1 t.cz[THREE+:TWO] = 6'b010011;
        #1 t.cz[FOUR-:TWO] = 6'b101011;

    end
endmodule
module top;
endmodule
