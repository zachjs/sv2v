package top_pkg;
    localparam AW = 16;
endpackage

package foo_pkg;
    typedef struct packed {
        logic valid;
        logic value;
    } mask_t;
    typedef struct packed {
        logic                   valid;
        logic [top_pkg::AW-1:0] user;
        mask_t                  mask;
    } boo_t;
endpackage

module top;
    foo_pkg::boo_t foo;
    assign foo = 19'b110_01001000_01110100;

    wire [7:0] bar;
    parameter FLAG = 1;
    assign bar = FLAG ? foo.user[0+:8] : '1;
endmodule
