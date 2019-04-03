module top;

integer x = 0;
integer y = 1;

`define GOOD \
    $display("%x %x %d", x, y, `__LINE__); \
    x += 1; $display(x);

`define BAD \
    $display("FAIL line ", `__LINE__);

initial begin

    `include "cond.vh"
    $display(`RESULT);
    `include "cond.vh"
    $display(`RESULT);
    `include "cond.vh"
    $display(`RESULT);
    `undef INCLUDED
    `include "cond.vh"
    $display(`RESULT);
    `include "cond.vh"
    $display(`RESULT);

    `define A
    `define C

    `ifdef A
        `GOOD
    `endif
    `ifndef A
        `BAD
    `endif

    `ifdef B
        `BAD
    `endif
    `ifndef B
        `GOOD
    `endif

    `ifdef A
        `GOOD
    `else
        `BAD
    `endif

    `ifdef B
        `BAD
        `define A
    `else
        `GOOD
    `endif

    `ifndef A
        `BAD
    `elsif B
        `BAD
    `elsif A
        `GOOD
    `else
        `BAD
    `endif

    `ifndef B
        `GOOD
    `elsif A
        `BAD
    `elsif B
        `BAD
    `else
        `BAD
    `endif

    `ifdef B
        `BAD
    `elsif A
        `GOOD
    `else
        `BAD
    `endif

    `ifdef B
        `BAD
    `elsif A
        `GOOD
    `elsif B
        `BAD
    `else
        `BAD
    `endif

    `ifdef B
        `BAD
    `elsif A
        `GOOD
    `elsif C
        `BAD
    `else
        `BAD
    `endif

    `ifdef A
        `ifdef B
            `BAD
            `ifdef B
                `BAD
            `else
                `BAD
            `endif
            `BAD
        `else
            `ifdef C
                `GOOD
                `ifndef C
                    `BAD
                `else
                    `GOOD
                `endif
            `else
                `BAD
            `endif
        `endif
        `GOOD
    `else
        `BAD
        `ifdef B
            `BAD
        `else
            `BAD
        `endif
        `BAD
    `endif

end

endmodule
