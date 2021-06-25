class C #(
    parameter X = 1
);
    static task dump;
        $display("C#(%0d)::dump()", X);
    endtask
endclass

package P;
    task dump;
        $display("P::dump()");
    endtask
endpackage

module top;
    task dump;
        $display("dump()");
    endtask

`define TEST(subroutine) \
    initial begin subroutine; end \
    initial begin subroutine(); end \
    initial begin ; subroutine; end \
    initial begin ; subroutine(); end

    `TEST(dump)
    `TEST(P::dump)
    `TEST(C#(1)::dump)

endmodule
