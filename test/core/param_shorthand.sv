module top #(FOO = 10);
    initial $display(FOO);
endmodule
module top2 #(FOO = 10, BAR = 11);
    initial $display(FOO, BAR);
endmodule
module top3 #(FOO = 10, BAR = 11, parameter BAZ = 12);
    initial $display(FOO, BAR, BAZ);
endmodule
