module Example;
    parameter [0:0] P = 0;
    initial $display("Example: %b", P);
endmodule

module top;
    Example      a();
    Example #(0) b();
    Example #(1) c();
    Example #(2) d();
    Example #(3) e();

    function [0:0] foo;
        input [0:0] inp;
        foo = inp ^ 1;
    endfunction
    initial begin
        $display("foo(0) = %b", foo(0));
        $display("foo(1) = %b", foo(1));
        $display("foo(2) = %b", foo(2));
        $display("foo(3) = %b", foo(3));
    end

    wire x;
    assign x = 1;
endmodule
