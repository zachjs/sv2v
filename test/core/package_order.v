module evil_mdl (
    output reg [evil_pkg_B-1:0] foo
);
    localparam evil_pkg_Z = 1;
    localparam evil_pkg_A = evil_pkg_Z;
    localparam evil_pkg_B = evil_pkg_Z;
    initial foo = evil_pkg_A;
endmodule

module top;
    localparam evil_pkg_Z = 1;
    localparam evil_pkg_A = evil_pkg_Z;
    localparam evil_pkg_B = evil_pkg_Z;
    wire [evil_pkg_B-1:0] foo;
    evil_mdl x(foo);
    initial $monitor(foo);
endmodule
