module top;
    localparam foo_pkg_Foo = 0;
    localparam foo_pkg_Goo = 1;
    initial begin
        $display(foo_pkg_Foo);
        $display(foo_pkg_Goo);
    end
endmodule

module top2;
    localparam Foo = 0;
    localparam Goo = 1;
    initial begin
        $display(Foo);
        $display(Goo);
    end
endmodule
