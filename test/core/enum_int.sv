package foo_pkg;
    typedef enum int {
        Foo = 0,
        Goo = 1
    } my_t;
endpackage

module top;
    initial begin
        $display(foo_pkg::Foo);
        $display(foo_pkg::Goo);
    end
endmodule

module top2;
    import foo_pkg::*;
    initial begin
        $display(Foo);
        $display(Goo);
    end
endmodule
