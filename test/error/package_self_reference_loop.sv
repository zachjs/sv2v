// pattern: package "P" references undeclared local "P::Foo"
package P;
    localparam Foo = P::Foo;
endpackage
module top;
    import P::*;
    initial $display(Foo);
endmodule
