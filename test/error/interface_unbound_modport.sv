interface Interface;
    logic x;
endinterface

module Module(i);
    Interface i;
endmodule

module top;
    Interface i();
    Module m();
endmodule
