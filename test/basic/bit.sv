module Example;
    parameter bit P = 0;
    initial $display("Example: %b", P);
endmodule

module top;
    Example      a();
    Example #(0) b();
    Example #(1) c();
    Example #(2) d();
    Example #(3) e();

    function bit foo;
        input bit inp;
        return inp ^ 1;
    endfunction
    initial begin
        $display("foo(0) = %b", foo(0));
        $display("foo(1) = %b", foo(1));
        $display("foo(2) = %b", foo(2));
        $display("foo(3) = %b", foo(3));
    end

    bit x;
    assign x = 1;
endmodule
