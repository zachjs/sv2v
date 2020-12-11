interface Interface(input x);
    initial $display("Hello!");
endinterface

module Module(i);
    Interface i;
    initial #1 $display(i.x);
endmodule

module top;
    Interface i1();
    Module m1(i1);
    Interface i2(.x());
    Module m2(i2);
endmodule
