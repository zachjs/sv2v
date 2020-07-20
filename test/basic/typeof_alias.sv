package P;
    typedef logic [1:0][2:0] T;
endpackage

module top;
    P::T w;
    type(w[0]) x;
    type(x[0]) y;
    type(w[0][0]) z;

    type(w[1:0]) a;
    type(w[0][1:0]) b;

    initial begin
        $display("%b %b %b %b", w, x, y, z);
        $display("%b %b", a, b);
    end
endmodule
