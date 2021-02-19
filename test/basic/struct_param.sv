module Module #(parameter type T, parameter N);
    T t;
    type(t.a) x;
    type(t.b) y;
    initial begin
        $display("$bits(T)=", $bits(T));
        $display("$bits(t)=", $bits(t));
        $display("$bits(t.a)=", $bits(t.a));
        $display("$bits(t.b)=", $bits(t.b));
        $display("$bits(x)=", $bits(x));
        $display("$bits(y)=", $bits(y));
        $display("$bits(N)=", $bits(N));
        $display("N=", N);
    end
endmodule
module top;
    typedef struct packed {
        logic a;
        logic [2] b;
    } Struct1;
    typedef struct packed {
        logic [5] a;
        Struct1 b;
        logic [2] c;
        logic d;
    } Struct2;
    Module #(Struct1, $bits(Struct1)) m1();
    Module #(Struct2, $bits(Struct2)) m2();
endmodule
