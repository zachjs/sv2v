task foo;
    $display("task foo() called");
endtask
function bar;
    input [2:0] n;
    bar = baz(n + 1);
endfunction
function baz;
    input [2:0] n;
    baz = n * 2;
endfunction
module top;
    initial foo();
    initial $display("bar(0) = %d", bar(0));
endmodule
