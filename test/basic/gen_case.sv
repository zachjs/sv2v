module Module;
    parameter X = 1;
    case (X)
        1: initial $display("A");
        2: initial $display("B");
        default: initial $display("C");
        3: ;
    endcase
endmodule

module top;
    Module #(1) a();
    Module #(2) b();
    Module #(3) c();
    Module #(4) d();
endmodule
