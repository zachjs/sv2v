`define TEST(value) \
    wire [63:0] val_``value = {64{1'b``value}}; \
    initial $display(`"'value -> %b (%0d) %b (%0d)", \
        val_``value, $bits(val_``value), \
        1'b``value, $bits(1'b``value) \
        );

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
    reg [63:0] j;
    reg [63:0] d;
    reg [63:0] e;
    initial begin
        i = 42;
        j = 42;
        flag = 1;
        a = (flag ? 32'hFFFFFFFF : i);
        b = (flag ? 32'hXXXXXXXX : i);
        c = (flag ?  32'hFFFFFFFF: i);
        d = (flag ? 64'hFFFFFFFFFFFFFFFF : j);
        e = (flag ? 64'hXXXXXXXXXXXXXXXX : j);
        $display("%b", a);
        $display("%b", b);
        $display("%b", c);
        $display("%b", d);
        $display("%b", e);
    end

    initial begin
        $display("%b", 4'b1xz0);
        $display("%b", {4'b1xz0, 4'b1xz0});
    end

    initial begin
        $display(1);
        $display(1);
        $display(1);
        $display(1);
    end
endmodule
