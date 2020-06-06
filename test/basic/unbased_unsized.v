`define TEST(value) \
    wire [63:0] val_``value = {64{1'b``value}}; \
    initial $display(`"'value -> %b %b", val_``value, 1'b``value);

module top;
    `TEST(1)
    `TEST(0)
    `TEST(x)
    `TEST(z)

    reg flag;
    reg [31:0] i;
    reg [31:0] a;
    reg [31:0] b;
    reg [31:0] c;
    initial begin
        i = 42;
        flag = 1;
        a = (flag ? 32'hFFFFFFFF : i);
        b = (flag ? 1'sb1 : i);
        c = (flag ?  32'hFFFFFFFF: i);
        $display("%b", a);
        $display("%b", b);
        $display("%b", c);
    end
endmodule
