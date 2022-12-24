// This verifies that sv2v can evaluate certain constant expressions by
// producing iverilog-incompatible code if the expression cannot be simplified
// or is evaluated incorrectly.
`define ASSERT_TRUE(expr) if (expr) begin end else begin shortreal x; end
`define ASSERT_FALSE(expr) if (expr) begin shortreal x; end

module top;
    `ASSERT_TRUE(1)
    `ASSERT_FALSE(0)

    `ASSERT_TRUE("inv" == "inv")
    `ASSERT_TRUE(32'("inv") == "inv")
    `ASSERT_TRUE("inv" == 32'("inv"))
    `ASSERT_TRUE(24'("inv") == "inv")
    `ASSERT_TRUE("inv" == 24'("inv"))

    `ASSERT_FALSE("invv" == "inv")
    `ASSERT_FALSE("0inv" == "inv")
    `ASSERT_TRUE("invv" != "inv")
    `ASSERT_TRUE("0inv" != "inv")

    `ASSERT_TRUE(24'("inv0") == "inv")
    `ASSERT_TRUE(24'("0inv") != "inv")
    `ASSERT_FALSE("inv" == 0)
    `ASSERT_FALSE("inv" == '0)
    `ASSERT_FALSE('0 == "inv")
    `ASSERT_FALSE("inv" == 1'b0)
    `ASSERT_FALSE("inv" == 2'd0)
    `ASSERT_FALSE("inv" == 1'sb0)

    `ASSERT_TRUE(1'sb0 < 1'd1)
    `ASSERT_TRUE(1'sb0 <= 1'd0)
    `ASSERT_FALSE(1'sb0 > 1'd1)
    `ASSERT_TRUE(1'sb0 >= 1'd0)

    `ASSERT_TRUE((1 | 2) == 3)
    `ASSERT_TRUE((13 & 7) == 5)

    `ASSERT_TRUE((1 << 1) == 2)
    `ASSERT_TRUE((3 >> 1) == 1)
    `ASSERT_TRUE((1 <<< 1) == 2)
    `ASSERT_TRUE((3 >>> 1) == 1)

    `ASSERT_TRUE(5'{4'hF, 3'{1'b1, 1'b1}} == 27)
    `ASSERT_TRUE(5'{{{1'b1, 1'b1}, {1'b1, 1'b1}}} == 15)
endmodule
