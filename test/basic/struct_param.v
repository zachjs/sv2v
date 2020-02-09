module Module1 #(parameter N = 1);
    initial begin
        $display("$bits(T)=", 3);
        $display("$bits(t)=", 3);
        $display("$bits(t.a)=", 1);
        $display("$bits(t.b)=", 2);
        $display("$bits(x)=", 1);
        $display("$bits(y)=", 2);
        $display("$bits(N)=", 32);
        $display("N=", N);
    end
endmodule
module Module2 #(parameter N = 1);
    initial begin
        $display("$bits(T)=", 11);
        $display("$bits(t)=", 11);
        $display("$bits(t.a)=", 5);
        $display("$bits(t.b)=", 3);
        $display("$bits(x)=", 5);
        $display("$bits(y)=", 3);
        $display("$bits(N)=", 32);
        $display("N=", N);
    end
endmodule
module top;
    Module1 #(3) m1();
    Module2 #(11) m2();
endmodule
