`define TEST(expr) \
    $display(`"expr = %b; $bits(expr) = %0d`", (expr), $bits(expr));

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

        // TODO: iverilog incorrectly handles width of these
        // `TEST(4'b1011 * 5'b01110)
        // `TEST(4'b1011 / 5'b01110)
        // `TEST(4'b1011 % 5'b01110)
        // `TEST(4'b1011 + 5'b01110)
        // `TEST(4'b1011 - 5'b01110)
        // `TEST(4'b1011 ** 5'b01110)

        // TODO: not yet supported by iverilog
        // `TEST(4'b1011 -> 5'b01110)
        // `TEST(4'b1011 <-> 5'b01110)
        // `TEST(4'b1011 ==? 5'b01110)
        // `TEST(4'b1011 !=? 5'b01110)

    end
endmodule
