module top;
    typedef struct packed {
        bit a, b;
    } T;

    localparam T FOO [4] = '{
        '{ 0, 0 },
        '{ 0, 1 },
        '{ 1, 0 },
        '{ 1, 1 }
    };

    initial begin
        $display(FOO[0].a);
        $display(FOO[0].b);
        $display(FOO[2].a);
        $display(FOO[2].b);
    end
endmodule
