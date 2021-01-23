// pattern: package dependency loop: "P" depends on "P"
package P;
    import P::*;
    localparam Foo = 1;
endpackage
module top;
    import P::*;
    initial $display(Foo);
endmodule
