module ModuleA #(P=1,) (inp,);
    input inp;
    initial $display("ModuleA P=%0d inp=%b", P, inp);
endmodule

module ModuleB #(parameter P,) (input inp,);
    initial $display("ModuleB P=%0d inp=%b", P, inp);
endmodule

module top;
    ModuleA #(1,) a(1'b1,);
    ModuleB #(.P(1),) b(.inp(1'b1),);
endmodule
