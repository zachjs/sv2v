`define FOO BAR
`define BAR(e) prefix``e
`define BAZ `"FOO`\`"`FOO`"
module top;
    initial begin
        $display("FOO");
        $display("`FOO");

        $display(`"`FOO`");
        $display(`"\`FOO`");
        $display(`"\\`FOO`");
        $display(`"\\\`FOO`");

        $display(`"\"FOO`");
        $display(`"\"`FOO`");
        $display(`"\"FOO\"`");
        $display(`"\"`FOO\"`");

        $display(`"FOO`");
        $display(`"FOO`FOO`");

        $display(`"`BAR(LOL)`");
        $display(`"\`BAR(LOL)`");
        $display(`"\\`BAR(LOL)`");
        $display(`"\\\`BAR(LOL)`");

        $display(`"`BAR(`FOO)`");
        $display(`"\`BAR(`FOO)`");
        $display(`"\\`BAR(`FOO)`");
        $display(`"\\\`BAR(`FOO)`");

        $display(`"`BAR(s`FOO)`");
        $display(`"\`BAR(s`FOO)`");
        $display(`"\\`BAR(s`FOO)`");
        $display(`"\\\`BAR(s`FOO)`");

        $display(`BAZ);

`ifdef DNE
        $display(`DNE);
        $display(`"`DNE`");
`define
`line
`foo
`endif

        $display("TEST END");
        $display("TEST\
 END");
        $display(`"TEST\
 END`");
        $display(`"TEST\`FOO\
 END`");
        $display(`"TEST\"`FOO\
 END`");
        $display(`"TEST\"`FOO\
 END`FOO`");
    end
endmodule
