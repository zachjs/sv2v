`define TEST(value) \
    logic [63:0] val_``value = 'value; \
    initial $display(`"'value -> %b %b", val_``value, 'value);

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
    initial begin
        i = 42;
        flag = 1;
        a = (flag ? '1 : i);
        b = (flag ? 1'sb1 : i);
        c = (flag ? '1 : '0);
        $display("%b", a);
        $display("%b", b);
        $display("%b", c);
    end
endmodule
