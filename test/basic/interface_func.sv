interface Foo;
    function bar;
        input integer x;
        return x * x;
    endfunction
endinterface

module top;
    Foo foo();
    initial $display(foo.bar(3));
endmodule
