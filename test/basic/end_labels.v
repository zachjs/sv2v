`define DUMP(expr, value) initial $display(`"expr = %0d`", value);

module top;
    `DUMP(W, 4)
    `DUMP(C#(3)::X, 3)
    `DUMP(C#(3)::Y, 6)
    `DUMP(P::Z, 3)
    `DUMP(f(3), 24)
    if (1) initial begin
        $display("intf");
        $display("Bye!");
        $display("Way!");
    end
    initial begin
        $display("t()");
        $display("Hello!");
        $display("No!");
    end
endmodule
