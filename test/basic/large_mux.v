module top;
    parameter SVO_MODE = "768x576";
    `include "large_mux.vh"
    wire [31:0] DOUBLE_SVO_HOR_PIXELS = 2 * SVO_HOR_PIXELS;
    initial begin
        $display("%s", SVO_MODE);
        $display("%d", SVO_HOR_PIXELS);
        $display("%d", DOUBLE_SVO_HOR_PIXELS);
    end
endmodule
