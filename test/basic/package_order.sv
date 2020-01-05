package evil_pkg;
    localparam Z = 1;
    localparam A = Z;
    localparam B = Z;

    function logic evil_fun;
        return A;
    endfunction
endpackage

module evil_mdl (
    output logic [evil_pkg::B-1:0] foo
);
    initial foo = evil_pkg::evil_fun();
endmodule

module top;
    logic [evil_pkg::B-1:0] foo;
    evil_mdl x(foo);
    initial $monitor(foo);
endmodule
