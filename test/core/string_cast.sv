`ifdef REF
    `define TEST(size) \
        begin : blk``size \
            reg [size - 1:0] trim; \
            trim = P; $display(`"size'(P) = %b`", trim); \
            trim = L; $display(`"size'(L) = %b`", trim); \
        end
`else
    `define TEST(size) \
        $display(`"size'(P) = %b`", size'(P)); \
        $display(`"size'(L) = %b`", size'(L));
`endif
module mod;
    parameter P = "asdf";
    localparam L = "foobar";
    initial begin
        `TEST(1) `TEST(2) `TEST(3) `TEST(4) `TEST(5)
        `TEST(6) `TEST(7) `TEST(8) `TEST(9) `TEST(10)
        `TEST(11) `TEST(12) `TEST(13) `TEST(14) `TEST(15)
        `TEST(16) `TEST(17) `TEST(18) `TEST(19) `TEST(20)
        `TEST(21) `TEST(22) `TEST(23) `TEST(24) `TEST(25)
        `TEST(26) `TEST(27) `TEST(28) `TEST(29) `TEST(30)
        `TEST(31) `TEST(32) `TEST(33) `TEST(34) `TEST(35)
        `TEST(36) `TEST(37) `TEST(38) `TEST(39) `TEST(40)
        `TEST(41) `TEST(42) `TEST(43) `TEST(44) `TEST(45)
        `TEST(46) `TEST(47) `TEST(48) `TEST(49) `TEST(50)
        `TEST(51) `TEST(52) `TEST(53) `TEST(54) `TEST(55)
    end
endmodule
