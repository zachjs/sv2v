module top;
    initial begin
        $display("intf.P.x %0d", 1);
        $display("intf.P.y %0d", 2);
        $display("intf.P.x %0d", 4);
        $display("intf.P.y %0d", 6);
        $display("intf.P.x %0d", 4);
        $display("intf.P.y %0d", 6);
        $display("mod.f.x %b", {8'd1, 16'd2});
        $display("mod.f.y %b", 32'h1);
        $display("mod.f.x %b", {8'd4, 16'd6});
        $display("mod.f.y %b", 32'hF);
    end
endmodule
