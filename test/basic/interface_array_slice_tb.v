module Test();
    parameter BASE = 0;
    parameter SIZE = 0;
    parameter DIR = 0;

    localparam LEFT = BASE;
    localparam RIGHT = BASE + DIR * (SIZE - 1);

    genvar left, right, offset;
    generate
        for (left = LEFT + SIZE; left <= RIGHT + SIZE; left = left + 1)
        for (right = LEFT + SIZE; right <= RIGHT + SIZE; right = right + 1)
        if ((left - right) * DIR <= 0)
        for (offset = -2 + SIZE; offset <= 2 + SIZE; offset = offset + 1)
        begin

        Instance #(
            LEFT, RIGHT,
            left - SIZE, right - SIZE, offset - SIZE
        ) i();

        initial begin
            i.xs = 1;
            while (i.xs != 0) begin
                #1;
                $display("LEFT=%2d RIGHT=%2d INNER_LEFT=%2d INNER_RIGHT=%2d INNER_OFFSET=%2d i.xs=%b i.l.xs=%b i.m.xs=%b i.n.xs=%b",
                    LEFT, RIGHT,
                    left - SIZE, right - SIZE, offset - SIZE,
                    i.xs, i.l.xs, i.m.xs, i.n.xs);
                i.xs = i.xs + 1;
            end
        end

        end
    endgenerate
endmodule

module Suite();
    parameter SIZE = 0;
    genvar base;
    generate
        for (base = -2 + SIZE; base <= 2 + SIZE; base = base + 1) begin
            Test #(base - SIZE, SIZE, -1) b();
            Test #(base - SIZE, SIZE, 1) f();
        end
    endgenerate
endmodule

module top;
    Suite #(2) s2();
    Suite #(3) s3();
    Suite #(4) s4();
endmodule
