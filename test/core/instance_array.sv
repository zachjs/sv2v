module Example;
    parameter FOO = 1;
    initial $display("%0d", FOO);
endmodule

module top;
    Example e[3][4:5]();
    defparam e[2][5].FOO = 1;
    defparam e[2][4].FOO = 2;
    defparam e[1][5].FOO = 4;
    defparam e[1][4].FOO = 8;
    defparam e[0][5].FOO = 16;
    defparam e[0][4].FOO = 32;
endmodule
