module mod;
    parameter `TYPE P = "not";
    localparam `TYPE L = "here";
    initial begin
        $display("mod.P = %s", P);
        $display("mod.L = %s", L);
    end
endmodule

module top;
    parameter `TYPE P = "param";
    localparam `TYPE L = "local";
    initial begin
        $display("top.P = %s", P);
        $display("top.L = %s", L);
    end
    mod m1();
    mod #("over") m2();
endmodule
