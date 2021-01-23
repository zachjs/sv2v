// pattern: invalid export Pkg::Foo outside of package
package Pkg;
    localparam Foo = 1;
endpackage
module top;
    import Pkg::Foo;
    export Pkg::Foo;
    initial $display(Foo);
endmodule
