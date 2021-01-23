// pattern: package dependency loop: "PkgA" depends on "PkgB", which depends on "PkgA"
package PkgA;
    import PkgB::Foo;
    export PkgB::Foo;
endpackage
package PkgB;
    import PkgA::Foo;
    export PkgA::Foo;
endpackage
module top;
    initial $display(PkgA::Foo);
endmodule
