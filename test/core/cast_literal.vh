`define TEST_ALL(literal) \
    `TEST(1, literal) `TEST(2, literal) `TEST(3, literal) `TEST(4, literal) `TEST(5, literal) \
    `TEST(6, literal) `TEST(7, literal) `TEST(8, literal) `TEST(9, literal) `TEST(10, literal) \
    `TEST(11, literal) `TEST(12, literal) `TEST(13, literal) `TEST(14, literal) `TEST(15, literal) \
    `TEST(16, literal) `TEST(17, literal) `TEST(18, literal) `TEST(19, literal) `TEST(20, literal) \
    `TEST(21, literal) `TEST(22, literal) `TEST(23, literal) `TEST(24, literal) `TEST(25, literal) \
    `TEST(26, literal) `TEST(27, literal) `TEST(28, literal) `TEST(29, literal) `TEST(30, literal) \
    `TEST(31, literal) `TEST(32, literal) `TEST(33, literal) `TEST(34, literal) `TEST(35, literal)

`TEST_ALL('d1)
`TEST_ALL('sd1)
`TEST_ALL(1'sd1)
`TEST_ALL(32'sd1)
`TEST_ALL('d123)
`TEST_ALL('sd123)
`TEST_ALL(14'sd123)
`TEST_ALL(15'sd123)

`TEST_ALL('b1)
`TEST_ALL('sb1)
`TEST_ALL('b10)
`TEST_ALL('sb10)
`TEST_ALL(1'b1)
`TEST_ALL(1'sb1)
`TEST_ALL(2'sb1)
`TEST_ALL(2'sb11)
`TEST_ALL(1'sbx)
`TEST_ALL(2'sbx)
`TEST_ALL(2'sb0x)
`TEST_ALL(2'sbx1)
`TEST_ALL(2'sbz0)

`TEST_ALL(1'so1)
`TEST_ALL(2'so1)
`TEST_ALL(4'so11)
`TEST_ALL(1'sox)
`TEST_ALL(2'sox)
`TEST_ALL(4'so0x)
`TEST_ALL(4'sox1)
`TEST_ALL(4'soz0)
`TEST_ALL(5'so0x)
`TEST_ALL(5'sox1)
`TEST_ALL(5'soz0)
`TEST_ALL(6'so0x)
`TEST_ALL(6'sox1)
`TEST_ALL(6'soz0)
`TEST_ALL(7'so0x)
`TEST_ALL(7'sox1)
`TEST_ALL(7'soz0)
`TEST_ALL('so7)
`TEST_ALL('so37)
`TEST_ALL('so47)
`TEST_ALL('so57)
`TEST_ALL('so07)
`TEST_ALL('o7)
`TEST_ALL('o37)
`TEST_ALL('o47)
`TEST_ALL('o57)
`TEST_ALL('o07)

`TEST_ALL('bx)
`TEST_ALL('ozx)
`TEST_ALL('hxz)
`TEST_ALL(1'bx)
`TEST_ALL(5'ozx)
`TEST_ALL(6'ozx)
`TEST_ALL(7'hxz)
`TEST_ALL('bzzz1)
`TEST_ALL('ozzz1)
`TEST_ALL('hzzz1)
`TEST_ALL('shf)
`TEST_ALL('sh6f)
`TEST_ALL('sh7f)
`TEST_ALL('sh8f)
`TEST_ALL('sh9f)
`TEST_ALL('sh0f)
`TEST_ALL('hf)
`TEST_ALL('h6f)
`TEST_ALL('h7f)
`TEST_ALL('h8f)
`TEST_ALL('h9f)
`TEST_ALL('h0f)

`TEST_ALL(1'ox)
`TEST_ALL(1'oz)
`TEST_ALL(4'ox0)
`TEST_ALL(4'ox1)
`TEST_ALL(4'ox2)
`TEST_ALL(4'ox3)
`TEST_ALL(4'ox4)
`TEST_ALL(4'ox5)
`TEST_ALL(4'ox6)
`TEST_ALL(4'ox7)
`TEST_ALL(4'oxx)
`TEST_ALL(4'oxz)
`TEST_ALL(4'oz0)
`TEST_ALL(4'oz1)
`TEST_ALL(4'oz2)
`TEST_ALL(4'oz3)
`TEST_ALL(4'oz4)
`TEST_ALL(4'oz5)
`TEST_ALL(4'oz6)
`TEST_ALL(4'oz7)
`TEST_ALL(4'ozx)
`TEST_ALL(4'ozz)
