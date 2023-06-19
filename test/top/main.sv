module top_a;
    initial $display("top_a");
    mod m();
endmodule
module top_b;
    initial $display("top_b");
    intf i();
    sub s(i);
endmodule
module mod;
    initial $display("mod");
endmodule
interface intf;
    initial $display("intf");
endinterface
module sub(interface i);
    initial $display("sub");
endmodule
module top_c;
    parameter type T;
    parameter U;
    initial if ($bits(T) == 1) $display("top_c");
endmodule
module top_d;
    initial $display("top_d");
endmodule
