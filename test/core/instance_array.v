module Example;
    parameter FOO = 1;
    initial $display("%0d", FOO);
endmodule

module top;
    Example e[5:0]();
    defparam e[0].FOO = 1;
    defparam e[1].FOO = 2;
    defparam e[2].FOO = 4;
    defparam e[3].FOO = 8;
    defparam e[4].FOO = 16;
    defparam e[5].FOO = 32;
endmodule
