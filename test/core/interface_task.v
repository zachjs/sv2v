module top;
    task i_x;
        input reg [31:0] i;
        $display("I x(%0d)", i);
    endtask
    reg [31:0] w = 31;
    reg [31:0] y = 42;
    task x;
        input reg [31:0] a, b;
        $display("x('{%0d, %0d})", a, b);
    endtask
    task automatic z;
        input reg [31:0] a, b;
        $display("z('{%0d, %0d})", a, b);
    endtask
    initial begin
        i_x(y);
        x(w, y);
        z(w, y);
    end
endmodule
