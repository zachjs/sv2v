module mod();
    parameter X = 0;
    parameter W = 0;
    reg [W-1:0] data;
    initial begin
        data = 1;
        $display("mod %0d A %b %0d", X, data, $bits(data));
        $display("mod %0d B %b %0d", X, mod.data, $bits(mod.data));
        $display("mod %0d C %b %0d", X, data, $bits(data));
        $display("mod %0d D %b %0d", X, mod.data, $bits(mod.data));
    end
endmodule
module top;
    mod #(1, 3) m1();
    mod #(2, 1) m2();
endmodule
