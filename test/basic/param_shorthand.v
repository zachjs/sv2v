module top #(parameter FOO = 10);
    initial $display(FOO);
endmodule
module top2 #(parameter FOO = 10, parameter BAR = 11);
    initial $display(FOO, BAR);
endmodule
module top3 #(parameter FOO = 10, parameter BAR = 11, parameter BAZ = 12);
    initial $display(FOO, BAR, BAZ);
endmodule
