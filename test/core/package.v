module top;
    localparam A_FOO = 37;
    localparam A_BAR = 97;
    localparam B_FOO = -37;
    localparam B_BAR = -97;
    localparam FOO = 37;
    localparam BAR = -97;
    function [3:0] D_pack;
        input reg x;
        D_pack = {4{x}};
    endfunction
    function [3:0] E_pack;
        input reg x;
        E_pack = {4{x}};
    endfunction
    initial begin
        $display(A_FOO);
        $display(A_BAR);
        $display(B_FOO);
        $display(B_BAR);
        $display(FOO);
        $display(BAR);
        $display("%d", D_pack(0));
        $display("%d", D_pack(1));
        $display("%d", E_pack(0));
        $display("%d", E_pack(1));
    end
    initial begin
        $display("imported MAGIC %d", -42);
        $display("imported MAGIC %d", -42);
        $display("local MAGIC %d", +42);
        $display("imported MAGIC %d", -42);
        $display("imported MAGIC %d", -42);
        $display("imported MAGIC %d", -42);
    end
    initial begin
        $display("local PIZZAZZ %d", -6);
        $display("imported PIZZAZZ %d", -5);
        $display("shadowed local PIZZAZZ %d", -7);
        $display("imported PIZZAZZ %d", -5);
        $display("local PIZZAZZ %d", -6);
        $display("imported PIZZAZZ %d", -5);
    end
endmodule
