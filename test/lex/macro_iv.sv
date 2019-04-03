// This file is the same macro.sv, except with cases removed for which my and
// iverilog's interpretations of specification differ. Below is a justification
// for each of these differences, with reference to the specification. All
// references are from Section 22.5.1 of IEEE 1800-2017.
//
// 1. Page 676 says both that the macro text continues until the first newline
// not preceded by a backslash, and that single line comments should not be
// included in the substituted text. My interpretation is that a macro line
// should be able to end in, say `// foo \`, and the macro should continue onto
// the next line. iverilog does not support such macro definitions.
//
// 2. Page 676 that macros with default arguments can be optionally separated by
// whitespace. I take whitespace to include newlines, but iverilog does not.
//
// 3. Page 676 explicitly stats that an argument may be default to empty by
// simply appending an '='. iverlog does not support such definitions.
//
// 4. Page 679 states that macro argument substitution should not be done inside
// of a string literal. iverilog does not obey this rule.

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
`define NUMBER_12 1337 /* stripped for iverilog compatibility */
`define NUMBER_13 1337 /* stripped for iverilog compatibility */

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
`define MACRO_A_06(str="") 1337 /* stripped for iverilog compatibility */
`define MACRO_A_07(str="") 1337 /* stripped for iverilog compatibility */

/* set a non-empty default for iverilog compatibility */
`define MACRO_B(s="baz") \
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

/* removed MACRO_E because iverlog performs escaping withing normal quotes */

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

    `MACRO_F(foo);
    `MACRO_F(display);

end
endmodule
