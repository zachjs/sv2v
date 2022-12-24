// pattern: non-void return inside task or void function
module top;
    function void f;
        return 1;
    endfunction
    initial f;
endmodule
