// pattern: package dependency loop: "P" depends on "P"
package P;
    localparam Foo = P::Foo;
endpackage
module top;
    import P::*;
    initial $display(Foo);
endmodule
