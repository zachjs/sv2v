interface intf;
    logic [2:0] data;
    modport m (output .data(data[0]));
endinterface
module mod(p);
    parameter X = 0;
    intf p;
    initial begin
        mod.p.data = 1;
        $display("mod %0d A %b %0d", X, p.data, $bits(p.data));
        $display("mod %0d B %b %0d", X, mod.p.data, $bits(mod.p.data));
        $display("mod %0d C %b %0d", X, p.data, $bits(logic [$bits(p.data):1]));
        $display("mod %0d D %b %0d", X, mod.p.data, $bits(logic [$bits(mod.p.data):1]));
    end
endmodule
module top;
    intf i();
    mod #(1) m1(i);
    mod #(2) m2(i.m);
endmodule
