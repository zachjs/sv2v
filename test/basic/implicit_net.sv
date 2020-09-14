`define DISPLAY(expr) \
    $display(`"expr = %b; $bits(expr) = %0d`", expr, $bits(expr));

`default_nettype wor

module Example(a, b, c);
    input a, b, c;
    initial begin
        `DISPLAY(a)
        `DISPLAY(b)
        `DISPLAY(c)
    end
endmodule

module top;
    assign foo = 0;
    type(foo) bar;
    assign bar = 1;

    and (o1, foo, bar);
    not (o2, o3, foo);
    not (u1, u2, u3);

    Example e (a, b, c);

    initial begin
        `DISPLAY(foo)
        `DISPLAY(bar)
        `DISPLAY(o1)
        `DISPLAY(o2)
        `DISPLAY(o3)
        `DISPLAY(u1)
        `DISPLAY(u2)
        `DISPLAY(u3)
        `DISPLAY(a)
        `DISPLAY(b)
        `DISPLAY(c)
    end
endmodule
