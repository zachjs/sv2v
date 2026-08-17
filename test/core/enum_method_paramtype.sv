// Enum methods on type-parameter fields (package alias and inline binding)
package p;
    typedef enum logic [1:0] { A, B, C, D } e_t;
    typedef struct packed { e_t f; } s_t;
endpackage

module inner_alias #(parameter type T = logic) ();
    T v;
    integer next_val;
    integer prev_val;
    initial begin
        next_val = v.f.next();
        prev_val = v.f.prev();
        $display("alias next=%0d prev=%0d", next_val, prev_val);
    end
endmodule

module inner_inline #(parameter type T = logic) ();
    T v;
    integer next_val;
    integer prev_val;
    initial begin
        next_val = v.f.next();
        prev_val = v.f.prev();
        $display("inline next=%0d prev=%0d", next_val, prev_val);
    end
endmodule

module inner_bare #(parameter type T = logic) ();
    T x;
    integer next_val;
    initial begin
        next_val = x.next();
        $display("bare next=%0d", next_val);
    end
endmodule

module top;
    inner_alias #(.T(p::s_t)) i();
    inner_inline #(.T(struct packed {
        enum logic [1:0] { W, X, Y, Z } f;
    })) j();
    inner_bare #(.T(p::e_t)) k();
endmodule
