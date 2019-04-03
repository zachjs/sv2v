`define NUMBER_01 42
`define NUMBER_02 (42)
`define NUMBER_03 (21 * 2)
`define NUMBER_04 21 * 2
`define NUMBER_05 21 *\
2
`define NUMBER_06 21 *\
    2
`define NUMBER_07 21 * /* foo */ 2
`define NUMBER_08 21 *\
    /* foo */\
    2
`define NUMBER_09 21 *\
    /* foo */  2
`define NUMBER_10 4 * \
10 + \
2
`define NUMBER_11 42 //
`define NUMBER_12 21 */* foo */2//foo
`define NUMBER_13 21 *\
    // foo \
    2

`define STRING_01 "foo\nbar"
`define STRING_02 " foo \n bar "

`define STRING_COMMAS_01 "test"
`define STRING_COMMAS_02 "%s", "test"
`define STRING_COMMAS_03 "%s%s", "te", "st"
`define STRING_COMMAS_04 "%s%s%s", "t", "e", "st"
`define STRING_COMMAS_05 "%s%s%s", \
    "t", "e", "st" \
    // FOO

`define MACRO_A_01(str="bar") str, str
`define MACRO_A_02(str= "bar") str, str
`define MACRO_A_03(str = "bar") str, str
`define MACRO_A_04( str = "bar") str, str
`define MACRO_A_05( str = "bar" ) str, str
`define MACRO_A_06( str
= "bar" ) str, str
`define MACRO_A_07( str
    = "bar" ) str, str

`define MACRO_B(s=) \
    $display(`MACRO_A_01(s));\
    $display(`MACRO_A_02(s));\
    $display(`MACRO_A_03(s));\
    $display(`MACRO_A_04(s));\
    $display(`MACRO_A_05(s));\
    $display(`MACRO_A_06(s));\
    $display(`MACRO_A_07(s));\
    $display(`MACRO_A_01( s));\
    $display(`MACRO_A_02( s));\
    $display(`MACRO_A_03( s));\
    $display(`MACRO_A_04( s));\
    $display(`MACRO_A_05( s));\
    $display(`MACRO_A_06( s));\
    $display(`MACRO_A_07( s));\
    $display(`MACRO_A_01 (s));\
    $display(`MACRO_A_02 (s));\
    $display(`MACRO_A_03 (s));\
    $display(`MACRO_A_04 (s));\
    $display(`MACRO_A_05 (s));\
    $display(`MACRO_A_06 (s));\
    $display(`MACRO_A_07 (s));\
    $display(`MACRO_A_01 (s ));\
    $display(`MACRO_A_02 (s ));\
    $display(`MACRO_A_03 (s ));\
    $display(`MACRO_A_04 (s ));\
    $display(`MACRO_A_05 (s ));\
    $display(`MACRO_A_06 (s ));\
    $display(`MACRO_A_07 (s ));\
    $display(`MACRO_A_01(\
        s\
    ));\
    $display(`MACRO_A_02(\
        s\
    ));\
    $display(`MACRO_A_03(\
        s\
    ));\
    $display(`MACRO_A_04(\
        s\
    ));\
    $display(`MACRO_A_05(\
        s\
    ));\
    $display(`MACRO_A_06(\
        s\
    ));\
    $display(`MACRO_A_07(\
        s\
    ));

`define MACRO_C(a, b=1) a, b

`define MACRO_D(display) $display(display);

`define MACRO_E(s) $display("s", `"s", `"ss");

`define MACRO_F(t) $display(`"s t = `\`"t`\`"");

module top;
initial begin

    $display("%d", `NUMBER_01);
    $display("%d", `NUMBER_02);
    $display("%d", `NUMBER_03);
    $display("%d", `NUMBER_04);
    $display("%d", `NUMBER_05);
    $display("%d", `NUMBER_06);
    $display("%d", `NUMBER_07);
    $display("%d", `NUMBER_08);
    $display("%d", `NUMBER_09);
    $display("%d", `NUMBER_10);
    $display("%d", `NUMBER_11);
    $display("%d", `NUMBER_12);
    $display("%d", `NUMBER_13);

    $display("%s", `STRING_01);
    $display("%s", `STRING_02);

    $display(`STRING_COMMAS_01);
    $display(`STRING_COMMAS_02);
    $display(`STRING_COMMAS_03);
    $display(`STRING_COMMAS_04);
    $display(`STRING_COMMAS_05);

    `MACRO_B();
    `MACRO_B("foo");

    $display(`MACRO_C("foo"));
    $display(`MACRO_C("foo",));
    $display(`MACRO_C("foo",2));

    `MACRO_D("display");

    `MACRO_E(foo);
    `MACRO_E(display);

    `MACRO_F(foo);
    `MACRO_F(display);

end
endmodule
