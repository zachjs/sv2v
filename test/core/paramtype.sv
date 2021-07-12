module m_def #(
    parameter type T = logic
);
    T x = 0;
    initial begin
        $display("m_def %b %b %d", x, x+1, $bits(T));
    end
endmodule

module m_nodef #(
    parameter type T
);
    T x = 0;
    initial begin
        $display("m_nodef %b %b %d", x, x+1, $bits(T));
    end
endmodule

module n_nodef #(
    parameter type T,
    parameter type U
);
    T x = 0;
    U y = 1;
    initial begin
        $display("n_nodef %b %b %d", x, x+1, $bits(T));
        $display("n_nodef %b %b %d", y, y+1, $bits(U));
    end
endmodule

module n_def #(
    parameter type T = logic,
    parameter type U = logic
);
    T x = 0;
    U y = 1;
    initial begin
        $display("n_def %b %b %d", x, x+1, $bits(T));
        $display("n_def %b %b %d", y, y+1, $bits(U));
    end
endmodule

module n_tdef #(
    parameter type T,
    parameter type U = logic
);
    T x = 0;
    U y = 1;
    initial begin
        $display("n_tdef %b %b %d", x, x+1, $bits(T));
        $display("n_tdef %b %b %d", y, y+1, $bits(U));
    end
endmodule

module o_nodef #(
    parameter a,
    parameter type T,
    parameter type U,
    parameter b
);
    T x = a;
    U y = b;
    initial begin
        $display("n_nodef a=%d %b %b %d", a, x, x+1, $bits(T));
        $display("n_nodef b=%d %b %b %d", b, y, y+1, $bits(U));
    end
endmodule

module p #(
    type T = logic, U = logic
);
    T x = 0;
    U y = 1;
    initial begin
        $display("p %b %b %0d", x, x+1, $bits(T));
        $display("p %b %b %0d", y, y+1, $bits(U));
    end
endmodule

module top; endmodule

// Top level modules appear to be generally instantiated in lexicographic order,
// but instances within a module do not. This silliness helps produce more
// consistent output.

module a_1; m_def x(); endmodule
module a_2; m_def #(logic [1:0]) x(); endmodule
module a_3; m_def #(.T(logic [1:0])) x(); endmodule

module b_1; m_nodef #(logic [1:0]) x(); endmodule
module b_2; m_nodef #(.T(logic [1:0])) x(); endmodule

module c_1; n_nodef #(logic [1:0], logic [2:0]) x(); endmodule
module c_2; n_nodef #(.T(logic [1:0]), .U(logic)) x(); endmodule
module c_3; n_nodef #(.U(logic), .T(logic [1:0])) x(); endmodule

module d_1; n_def #(logic [1:0], logic [2:0]) x(); endmodule
module d_2; n_def #(.T(logic [1:0])) x(); endmodule
module d_3; n_def #(.U(logic [1:0])) x(); endmodule
module d_4; n_def #(.U(logic), .T(logic [1:0])) x(); endmodule
module d_5; n_def x(); endmodule

module e_1; n_tdef #(logic [1:0], logic [2:0]) x(); endmodule
module e_2; n_tdef #(.T(logic [1:0]), .U(logic)) x(); endmodule
module e_3; n_tdef #(.U(logic), .T(logic [1:0])) x(); endmodule

module f_1; o_nodef #(1, logic [1:0], logic [2:0], 0) x(); endmodule
module f_2; o_nodef #(.T(logic [1:0]), .U(logic), .b(1), .a(0)) x(); endmodule
module f_3; o_nodef #(0, logic [1:0], logic [2:0], 1) x(); endmodule
module f_4; o_nodef #(.T(logic [1:0]), .U(logic), .b(0), .a(1)) x(); endmodule

function automatic integer square;
    input integer inp;
    return inp ** 2;
endfunction

localparam type SomeT = logic [square(integer'(3)):0] ;
localparam type OtherT = enum SomeT { A, B, C, D };

module p_1; p #(logic [1:0], logic [2:0]) x(); endmodule
module p_2; p x(); endmodule
module p_3; localparam W = 2; p #(logic [W:0], logic [W:0]) x(); endmodule
module p_4; parameter W = 2; p #(logic [$clog2(W):0], logic [$clog2(W):0]) x(); endmodule
module p_5; parameter W = 2; p #(logic [$unsigned(W):0], logic [$unsigned(W):0]) x(); endmodule
module p_6; parameter W = 2; p #(logic [$signed(W):0], logic [$signed(W):0]) x(); endmodule
module p_7; parameter W = 2; p #(logic [square(W):0], logic [square(W):0]) x(); endmodule
module p_8; p #(OtherT) x(); endmodule
module p_9; p #(SomeT, OtherT) x(); endmodule
module p_A; if (1) begin : blk p #(SomeT, OtherT) x(); end endmodule
