// pattern: missing expected `endfunction`
module top;
    function automatic foo;
        input inp;
        foo = inp;
endmodule
