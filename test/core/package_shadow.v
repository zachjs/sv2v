module top;
    localparam P_X = 10;
    localparam P_Y = P_X + 1;
    localparam X = 20;
    localparam Z = P_Y + 1;
    localparam R_Y = P_X + 1000;
    localparam Q_Y = R_Y;
    localparam [3:0] T = 3;
    localparam [3:0] U = 4;
    function integer flip;
        input integer inp;
        flip = ~inp;
    endfunction
    function flop;
        input inp;
        flop = ~inp;
    endfunction
    initial begin
        $display("X %0d", X);
        $display("P::Y %0d", P_Y);
        $display("Z %0d", Z);
        $display("R::Y %0d", Q_Y);
        $display("flip(0) %0d", flip(0));
        $display("T %b", T);
        $display("U %b", U);
        $display("flop(0) %b", flop(0));
        $display("flop(1) %b", flop(1));
    end
endmodule
