interface Interface;
    logic x;
endinterface

module Module(i, j);
    Interface i;
    logic j;
    assign i.x = j.x;
endmodule

module top;
    Interface i();
    Interface j();
    Module m(i, j);
endmodule
