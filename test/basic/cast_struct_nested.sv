module top;
    typedef struct packed {
        logic x, y;
    } S;
    typedef struct packed {
        S x, y;
    } T;
    T t;
    initial begin
        t = 1'sb1;
        $display("%b", t);
        $display("%b", 5'(t));
    end
endmodule
