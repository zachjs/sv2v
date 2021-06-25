interface Interface;
    logic x;
endinterface

module Module (
    Interface p
);
    typedef struct packed {
        integer p;
    } S;
    S s = '{ p: 1 };
    initial $display("%0d %0d", s, s.p);
endmodule

module top;
    Interface intf();
    Module mod(intf);
endmodule
