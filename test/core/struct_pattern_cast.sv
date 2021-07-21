module top;
    typedef struct packed {
        integer x;
        byte y;
    } S;
    typedef struct packed {
        byte x;
        shortint y;
        S z;
    } T;
    var T a;
    initial a = T'{ x: 5, y: 6, z: '{ x: 7, y: 8 } };
    var T b;
    T c;
    assign c = T'{ x: 9, y: 10, z: '{ x: 11, y: 12 } };
    initial begin
        b = T'{ x: 13, y: 14, z: '{ x: 15, y: 16 } };
        #1;
        $display("a %b", a);
        $display("b %b", b);
        $display("c %b", c);
        $display("x %b", T'{ x: 1, y: 2, z: '{ x: 3, y: 4 } });
        $display("$bits(S) = %0d", $bits(S));
        $display("$bits(T) = %0d", $bits(T));
        $display("$bits(a) = %0d", $bits(a));
        $display("$bits(a.x) = %0d", $bits(a.x));
        $display("$bits(a.y) = %0d", $bits(a.y));
        $display("$bits(a.z) = %0d", $bits(a.z));
        $display("$bits(a.z.x) = %0d", $bits(a.z.x));
        $display("$bits(a.z.y) = %0d", $bits(a.z.y));
    end
endmodule
