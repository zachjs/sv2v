module top;
    typedef struct packed {
        logic a;
        logic b;
    } foo_t;
    foo_t foo;
    initial begin
        foo = '{a: 1'b1, default: '0};
        $display(foo, foo.a, foo.b);
    end
endmodule
