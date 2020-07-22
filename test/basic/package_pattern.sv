package PKG;
    typedef struct packed {
        logic f;
    } foo_t;
endpackage

module top;
    typedef struct packed {
        PKG::foo_t f;
    } local_t;
    local_t w[0:1];
    initial begin
        w <= '{default: 0};
        $display("%b", w);
    end
endmodule
