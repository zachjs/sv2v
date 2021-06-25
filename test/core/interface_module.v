module ModuleC;
    wire y;
    assign y = 1;
    initial $display("ModuleB %b", y);
    wire x;
    assign x = 0;
    initial $display("ModuleA %b", x);
endmodule

module top;
    ModuleC module_c();
endmodule
