module top;
    parameter WIDTH = 1;
    if (1) begin : a
        localparam tmp = WIDTH * 2;
        if (1) begin : c
            localparam WIDTH = tmp * 3;
            reg [WIDTH-1:0] x;
        end
    end
    if (1) begin : b
        localparam tmp = WIDTH * 5;
        if (1) begin : d
            localparam WIDTH = tmp * 7;
            reg [WIDTH-1:0] x;
        end
    end
    reg [$bits(a.c.x):0] a_c_x;
    reg [$bits(b.d.x):0] b_d_x;
    if (1) begin : e
        localparam tmp = WIDTH * 11;
        if (1) begin : f
            localparam WIDTH = tmp * 13;
            reg [WIDTH-1:0] x;
            reg [$bits(a.c.x):0] a_c_x;
            reg [$bits(b.d.x):0] b_d_x;
            initial begin
                a_c_x = 1;
                b_d_x = 1;
                $display("B a.c.x %b", a.c.x);
                $display("B a_c_x %b", a_c_x);
                $display("B b.d.x %b", b.d.x);
                $display("B b_d_x %b", b_d_x);
            end
        end
    end
    reg [$bits(e.f.x):0] e_f_x;
    reg [$bits(e.f.a_c_x):0] e_f_a_c_x;
    reg [$bits(e.f.b_d_x):0] e_f_b_d_x;
    initial begin
        e_f_x = 1'sb1;
        e_f_a_c_x = 1'sbx;
        e_f_b_d_x = 1'sbz;
        $display("A a.c.x %b", a.c.x);
        $display("A a_c_x %b", a_c_x);
        $display("A b.d.x %b", b.d.x);
        $display("A b_d_x %b", b_d_x);
        $display("A e.f.x %b", e.f.x);
        $display("A e_f_x %b", e_f_x);
        $display("A e.f.a_c_x %b", e.f.a_c_x);
        $display("A e_f_a_c_x %b", e_f_a_c_x);
        $display("A e.f.b_d_x %b", e.f.b_d_x);
        $display("A e_f_b_d_x %b", e_f_b_d_x);
    end
endmodule
