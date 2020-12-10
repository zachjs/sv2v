`define TEST(N) \
    $display(`"N -> %0d %b`", N, N); \
    $display(`"$bits(N) -> %0d`", $bits(N));

module top;
    initial begin
        `TEST(0) `TEST(1) `TEST(2)
        `TEST(-0) `TEST(-1) `TEST(-2)
        `TEST('d0) `TEST('d1) `TEST('d2)
        `TEST('sd0) `TEST('sd1) `TEST('sd2)
        `TEST('b0) `TEST('b1) `TEST('b10)
        `TEST('sd0) `TEST('sd1) `TEST('sd2)
        `TEST(1'sox) `TEST(2'sox) `TEST(3'sox) `TEST(7'sox) `TEST(8'sox) `TEST(9'sox) `TEST(9'soxx) `TEST(10'soxx)
        `TEST(1'soz) `TEST(2'soz) `TEST(3'soz) `TEST(7'soz) `TEST(8'soz) `TEST(9'soz) `TEST(9'sozz) `TEST(10'sozz)
        `TEST(1'SOZ) `TEST(2'SOZ) `TEST(3'SOZ) `TEST(7'SOZ) `TEST(8'SOZ) `TEST(9'SOZ) `TEST(9'SOZZ) `TEST(10'SOZZ)

        `TEST(1234_5678) `TEST('h1234_5678) `TEST('o1234_5677) `TEST('b0101_1100)
        `TEST('d4294967295) `TEST('d4294967296) `TEST('d4294967297) `TEST('d4294967298) `TEST('d4294967299)
        `TEST('d004294967295) `TEST('d004294967296) `TEST('d004294967297) `TEST('d004294967298) `TEST('d004294967299)

        `TEST(4294967295) `TEST(4294967296) `TEST(4294967297) `TEST(4294967298) `TEST(4294967299)
        `TEST(-4294967295) `TEST(-4294967297) `TEST(-4294967298) `TEST(-4294967299)
        `TEST(-8589934593) `TEST(8589934592) `TEST(8589934593)
        // iverlog does weird things with these: `TEST(-4294967296) `TEST(-8589934592)

        `TEST(659) `TEST('h 837FF) `TEST('o7460)
        `TEST(4'b1001) `TEST(5 'D 3) `TEST(3'b01x) `TEST(12'hx) `TEST(16'hz)
        `TEST(-8 'd 6) `TEST(4 'shf) `TEST(-4 'sd15) `TEST(16'sd?)

        `TEST('bx) `TEST('bz) `TEST('bzx) `TEST('bxz)
        `TEST(3'bx) `TEST(3'b1x) `TEST(3'bx1) `TEST('b1x) `TEST('bx1) `TEST(3'b0x1) `TEST(3'b0z1)
        `TEST('hf & 10'hf) `TEST(7'hf & 10'hf)

        `TEST('b01xz01xz01xz01xz01xz01xz01xz01xz01xz) `TEST('b101xz01xz01xz01xz01xz01xz01xz01xz01xz)
        `TEST(36'b01xz01xz01xz01xz01xz01xz01xz01xz01xz) `TEST(37'b01xz01xz01xz01xz01xz01xz01xz01xz01xz)
        `TEST(36'sb01xz01xz01xz01xz01xz01xz01xz01xz01xz) `TEST(37'sb01xz01xz01xz01xz01xz01xz01xz01xz01xz)
        `TEST('h01xz01xz) `TEST('h101xz01xz)
        `TEST(36'h01xz01xz) `TEST(37'h01xz01xz)
        `TEST(36'hb01xz01xz) `TEST(37'hb01xz01xz)

        `TEST('sb0) `TEST('sb1)
        `TEST('sb00) `TEST('sb10) `TEST('sb01) `TEST('sb11)
        `TEST('sb000) `TEST('sb001) `TEST('sb010) `TEST('sb011)
        `TEST('sb100) `TEST('sb101) `TEST('sb110) `TEST('sb111)

        `TEST('b0) `TEST('b1)
        `TEST('b00) `TEST('b10) `TEST('b01) `TEST('b11)
        `TEST('b000) `TEST('b001) `TEST('b010) `TEST('b011)
        `TEST('b100) `TEST('b101) `TEST('b110) `TEST('b111)

        `TEST('b0x) `TEST('sb0x) `TEST('b0z) `TEST('sb0z)
        `TEST('o0x) `TEST('so0x) `TEST('o0z) `TEST('so0z)
        `TEST('h0x) `TEST('sh0x) `TEST('h0z) `TEST('sh0z)
    end
endmodule
