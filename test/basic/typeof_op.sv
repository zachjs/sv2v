`define TEST(expr) \
    $display("%s = %b; $bits(%s) = %0d", `"expr`", expr, `"expr`", $bits(expr));

module top;
    initial begin

        `TEST(4'b1011)
        `TEST(5'b01110)

        `TEST(! 3'b101)
        `TEST(~ 3'b101)
        `TEST(+ 3'b101)
        `TEST(- 3'b101)
        `TEST(& 3'b101)
        `TEST(~& 3'b101)
        `TEST(| 3'b101)
        `TEST(~| 3'b101)
        `TEST(^ 3'b101)
        `TEST(~^ 3'b101)

        `TEST(4'b1011 && 5'b01110)
        `TEST(4'b1011 || 5'b01110)
        `TEST(4'b1011 & 5'b01110)
        `TEST(4'b1011 ^ 5'b01110)
        `TEST(4'b1011 ~^ 5'b01110)
        `TEST(4'b1011 | 5'b01110)
        `TEST(4'b1011 << 5'b01110)
        `TEST(4'b1011 >> 5'b01110)
        `TEST(4'b1011 <<< 5'b01110)
        `TEST(4'b1011 >>> 5'b01110)
        `TEST(4'b1011 == 5'b01110)
        `TEST(4'b1011 != 5'b01110)
        `TEST(4'b1011 === 5'b01110)
        `TEST(4'b1011 !== 5'b01110)
        `TEST(4'b1011 < 5'b01110)
        `TEST(4'b1011 <= 5'b01110)
        `TEST(4'b1011 > 5'b01110)
        `TEST(4'b1011 >= 5'b01110)

        `TEST(4'b1011 * 5'b01110)
        `TEST(4'b1011 / 5'b01110)
        `TEST(4'b1011 % 5'b01110)
        `TEST(4'b1011 + 5'b01110)
        `TEST(4'b1011 - 5'b01110)
        `TEST(4'b1011 ** 5'b01110)
        `TEST(4'b1011 -> 5'b01110)
        `TEST(4'b1011 <-> 5'b01110)
        `TEST(4'b1011 ==? 5'b01110)
        `TEST(4'b1011 !=? 5'b01110)

    end
endmodule
