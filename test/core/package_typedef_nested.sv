package foo_pkg;

    typedef struct packed {
        logic [7:0] rsvd;
        logic [7:0] parity;
    } user_t;

    typedef struct packed {
        logic  valid;
        user_t user;
    } inp_t;

    typedef struct packed {
        logic valid;
        logic opcode;
    } out_t;

endpackage

module top (
    input  foo_pkg::inp_t dat_i,
    output foo_pkg::out_t dat_o
);
endmodule
