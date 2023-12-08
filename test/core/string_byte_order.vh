`define TEST(str) initial \
    $display("%s %b %c %c", str, str, str[0], str[3]);
`TEST(a)
`TEST(b)
`TEST(c)
`TEST(d)
`TEST(e)
