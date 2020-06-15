interface I;
    parameter WIDTH = 32;
    logic [WIDTH-1:0] data = 0;
    modport P(input data);
endinterface

module M(i);
    parameter A = 1;
    I.P i;
    parameter B = 2;
    initial begin
        $display("A %b", A);
        $display("I.P %b", i.data);
        $display("B %b", B);
    end
endmodule

module top;
    I x();
    I #(10) y();
    M a(x);
    M b(y);
    M #(3, 4) c(x);
    M #(5, 6) d(y);
endmodule
