interface Foo;
    parameter MULTIPLIER = 7;
    function integer bar;
        input integer x;
        return x * x * MULTIPLIER;
    endfunction
endinterface

module top;
    Foo foo_1();
    Foo #(.MULTIPLIER(8)) foo_2();
    initial begin
        $display(foo_1.bar(3));
        $display(foo_2.bar(3));
    end
endmodule
