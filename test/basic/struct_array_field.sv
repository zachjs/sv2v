module main;
    typedef struct packed {
        logic [1:0][2:0] x;
        logic [0:2][1:0] y;
        logic z;
    } foo_t;
    foo_t foo;

    initial begin
        $monitor($time, " %b %b %b %b %b %b %b %b",
            foo, foo.x, foo.y, foo.z,
            foo.x[0], foo.x[0][0], foo.y[0], foo.y[0][0]);

        #1; foo.z = 0;

        #1; foo.y = 0;
        #1; foo.y[0] = '1;
        #1; foo.y[1] = '1;
        #1; foo.y[1][1] = 0;
        #1; foo.y[0][0] = 1;
        #1; foo.y[0][1] = 1;

        #1; foo.x = 0;
        #1; foo.x[0] = '1;
        #1; foo.x[1] = '1;
        #1; foo.x[1][1] = 0;
        #1; foo.x[0][0] = 1;
        #1; foo.x[0][1] = 1;

    end
endmodule

module top;
endmodule
