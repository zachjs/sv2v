`define TEST(value) \
    logic [63:0] val_``value = 'value; \
    initial $display(`"'value -> %b (%0d) %b (%0d)", \
        val_``value, $bits(val_``value), \
        'value, $bits('value) \
        );

module top;
    `TEST(1);
    `TEST(0);
    `TEST(x);
    `TEST(z);

    logic flag;
    logic [31:0] i;
    logic [31:0] a;
    logic [31:0] b;
    logic [31:0] c;
    logic [63:0] j;
    logic [63:0] d;
    logic [63:0] e;
    initial begin
        i = 42;
        j = 42;
        flag = 1;
        a = (flag ? '1 : i);
        b = (flag ? 'x : i);
        c = (flag ? '1 : '0);
        d = (flag ? '1 : j);
        e = (flag ? 'x : j);
        $display("%b", a);
        $display("%b", b);
        $display("%b", c);
        $display("%b", d);
        $display("%b", e);
    end

    initial begin
        $display("%b", {'1, 'x, 'z, '0});
        $display("%b", {2 {'1, 'x, 'z, '0}});
    end

    initial begin
        $display($bits('1));
        $display($bits(flag ? '1 : 'x));
        $display($bits(type('1)));
        $display($bits(type(flag ? '1 : 'x)));
    end
endmodule
