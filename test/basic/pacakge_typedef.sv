package top_pkg;
    localparam AW = 16;
endpackage

package foo_pkg;
    typedef struct packed {
        logic                   valid;
        logic [top_pkg::AW-1:0] user;
    } boo_t;
endpackage

module top;
    foo_pkg::boo_t foo;
endmodule
