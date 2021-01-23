// pattern: package dependency loop: "Pkg" depends on "Pkg"
package Pkg;
    localparam Foo = 1;
    export Pkg::Foo;
endpackage
module top;
    initial $display(Pkg::Foo);
endmodule
