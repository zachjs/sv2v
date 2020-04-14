module top;
    logic x = 1;
    foo f(x);
endmodule

interface foo(input logic x);
    bar a(x);
    bar b(~x);
endinterface

interface bar(input logic x);
    initial begin
        $display("bar got %b", x);
    end
endinterface
