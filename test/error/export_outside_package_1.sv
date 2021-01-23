// pattern: invalid export Pkg::Foo outside of package
package Pkg;
    localparam Foo = 1;
endpackage
import Pkg::Foo;
export Pkg::Foo;
module top;
    initial $display(Foo);
endmodule
