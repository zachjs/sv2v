// pattern: could not find "Foo" in package "Bar"
package Bar;
    localparam Bar = 1;
endpackage
package Pkg;
    import Bar::*;
    export Bar::Foo;
endpackage
module top;
    initial $display(Pkg::Foo);
endmodule
