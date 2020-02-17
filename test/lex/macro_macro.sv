`define SUFFIX _MAGIC
`define CHOICE_FOO 1
`define CHOICE_BAR 2
`define CHOICE_FOO_MAGIC 3
`define CHOICE_BAR_MAGIC 4
`define CHOICE__MAGIC 5
`define MACRO1(A, B) \
    `CHOICE_``A , `CHOICE_``B
`define MACRO2(A, B) \
    `CHOICE_``A```SUFFIX , `CHOICE_``B```SUFFIX
`define MACRO3 \
    `CHOICE_```SUFFIX , `CHOICE_```SUFFIX
module top;
    initial begin
        $display(`MACRO1(FOO, BAR));
        $display(`MACRO2(FOO, BAR));
        $display(`MACRO3);
    end
endmodule
