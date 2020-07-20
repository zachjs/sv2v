package PKG;
    typedef struct packed {
        logic f;
    } foo_t;
endpackage

module top;
    typedef struct packed {
        PKG::foo_t f;
    } local_t;
    local_t w;
    initial begin
        w <= local_t'(1'sb1);
        $display("%b", w);
    end
endmodule
