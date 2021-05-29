module other;
    parameter STR = "missing";
    initial $display("other: STR=%s $bits(STR)=%0d", STR, $bits(STR));
endmodule

module mod;
    parameter STR = "missing";
    initial $display("mod: STR=%s", STR);
    other #("HI") m();
endmodule
