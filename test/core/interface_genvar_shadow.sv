interface intf;
    parameter P;
    logic [P - 1:0] x;
    assign x = P;
    initial $display("intf %b", x);
endinterface
module mod(intf intf[4]);
    genvar i;
    for (i = 0; i < 4; i++) begin : blk
        for (genvar i = 0; i < 2; i++) begin : blk
            initial $display("foo %0d", i);
        end
        initial $display("mod [%0d] %b", i, intf[i].x);
        initial begin
            localparam intf = "shadowed_intf";
            $display("mod %s %0d %b", intf, i, mod.intf[i].x);
        end
    end
    for (i = 0; i < 2; i++) begin
        initial $display("bar %0d", i);
    end
endmodule
module top;
    for (genvar i = 1; i <= 8; i *= 2) begin : blk
        intf #(i) intf[4]();
        mod mod(intf);
    end
endmodule
