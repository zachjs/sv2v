// pattern: export of Bar::Foo differs from import of Foo::Foo
package Bar;
    localparam Bar = 1;
    localparam Foo = 3;
endpackage
package Foo;
    localparam Foo = 2;
endpackage
package Pkg;
    import Foo::Foo;
    import Bar::Bar;
    export Bar::Foo;
endpackage
module top;
    initial $display(Pkg::Foo);
endmodule
