`begin_keywords "1364-1995"
task foo;
    integer automatic = 2;
    $display(automatic * automatic);
endtask
`begin_keywords "1364-2005"
task automatic bar;
    integer logic = 3;
    $display(logic * logic);
endtask
`end_keywords
`end_keywords
`begin_keywords "1364-2001-noconfig"
task baz;
    integer cell = 3;
    $display(cell ** cell);
endtask
`end_keywords
module top;
    initial foo;
    initial bar;
    initial baz;
endmodule
