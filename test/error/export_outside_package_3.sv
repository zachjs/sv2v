// pattern: invalid export \*::\* outside of package
package Pkg;
    localparam Foo = 1;
endpackage
import Pkg::Foo;
export *::*;
module top;
    initial $display(Foo);
endmodule
