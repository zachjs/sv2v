typedef struct packed { logic x; } T;

interface Interface;
    integer x;
    function z;
        input x;
        z = ~x;
    endfunction
endinterface

module Module(interface y);
    function z;
        input T y;
        z = y.x;
    endfunction
    integer x = 0;
    initial begin
        #1;
        $display("x = %b", x);
        $display("z(x) = %b", z(x));
        $display("y.x = %b", y.x);
        $display("y.z(x) = %b", y.z(x));
        $display("y.z(y.x) = %b", y.z(y.x));
        $display("y.z(z(y.x)) = %b", y.z(z(y.x)));
    end
endmodule

module top;
    Interface x();
    initial x.x = 1;
    Module y(x);
endmodule
