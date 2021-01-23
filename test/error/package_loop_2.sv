// pattern: package dependency loop: "PkgA" depends on "PkgC", which depends on "PkgB", which depends on "PkgA"
package PkgA;
    import PkgC::Foo;
    export PkgC::Foo;
endpackage
package PkgB;
    import PkgA::Foo;
    export PkgA::Foo;
endpackage
package PkgC;
    import PkgB::Foo;
    export PkgB::Foo;
endpackage
module top;
    initial $display(PkgA::Foo);
endmodule
