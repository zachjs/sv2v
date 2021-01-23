// pattern: export of PkgA::Bar, but Bar was never imported
package PkgA;
    localparam Bar = 2;
endpackage
package PkgB;
    export PkgA::Bar;
    localparam Foo = 1;
endpackage
module top;
    initial $display(PkgB::Foo);
endmodule
