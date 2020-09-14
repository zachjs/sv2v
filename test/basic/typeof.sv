module top;
    function f;
        input x;
        f = 1'b1 ^ x;
        $display("f(%b) called", x);
    endfunction
    task t;
        input x;
        $display("t(%b) called", x);
    endtask

    initial begin
        type(f(0)) x = f(0);
        type(x) y = ~x;
        $display("%b", x);
        $display("%b", y);
        $display("%b", $bits(x));
        $display("%b", $bits(type(x)));
        $display("%b", $bits(logic [0:1+$bits(type(x))]));
        f(1);
        void'(f(0));
        t(1);
    end

    parameter FLAG = 1;
    initial begin
        logic [4:1] x = 4'b1011;
        type(x ^ 3'b111) y = x ^ 3'b111;
        type(x ^ 5'b11111) z = x ^ 5'b11111;
        type({8 {x}}) a = {8 {x}};
        type({x, y}) b = {x, y};
        type(FLAG ? x : y) c = FLAG ? x : y;
        type(!FLAG ? x : y) d = !FLAG ? x : y;
        type($clog2(x)) e = $clog2(x);
        type(!e) f = !e;
        $display("%b %d %d", x, $left(x), $right(x));
        $display("%b %d %d", y, $left(y), $right(y));
        $display("%b %d %d", z, $left(z), $right(z));
        $display("%b %d %d", a, $left(a), $right(a));
        $display("%b %d %d", b, $left(b), $right(b));
        $display("%b %d %d", c, $left(c), $right(c));
        $display("%b %d %d", d, $left(d), $right(d));
        $display("%b %d %d", e, $left(e), $right(e));
        $display("%b %d", f, $bits(f));
    end

    parameter W = 4;
    initial begin
        type('1) w = '1;
        logic [W-1:0] x = 4'hA;
        type(FLAG ? x : '1) y = FLAG ? x : '1;
        type(!FLAG ? y : '1) z = !FLAG ? y : '1;
        $display("%b %d %d", w, $left(w), $right(w));
        $display("%b %d %d", x, $left(x), $right(x));
        $display("%b %d %d", y, $left(y), $right(y));
        $display("%b %d %d", z, $left(z), $right(z));
    end

    initial begin
        type(1) w = 1;
        type(-1) x = -1;
        type(32'hffff_ffff) y = 32'hffff_ffff;
        type(32'shffff_ffff) z = 32'shffff_ffff;
        $display("%b %d %d %d", w, w, $left(w), $right(w));
        $display("%b %d %d %d", x, x, $left(x), $right(x));
        $display("%b %d %d %d", y, y, $left(y), $right(y));
        $display("%b %d %d %d", z, z, $left(z), $right(z));
    end

    for (genvar i = 0; i < 2; ++i)
        initial begin
            type(i) a;
            a = ~i;
            $display("%b %d %d %d", i, i, $left(i), $right(i));
            $display("%b %d %d %d", a, a, $left(a), $right(a));
        end

    localparam X = 5'b10110;
    localparam Y = X + 6'b00001;
    initial begin
        type(X) tX = X;
        type(Y) tY = Y;
        $display("%b %d %d %d", X, X, $left(X), $right(X));
        $display("%b %d %d %d", Y, Y, $left(Y), $right(Y));
        $display("%b %d %d %d", tX, tX, $left(tX), $right(tX));
        $display("%b %d %d %d", tY, tY, $left(tY), $right(tY));
    end
endmodule
