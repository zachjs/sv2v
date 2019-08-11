`define FOO(a, b) ((a)+(b))

module top;
    initial begin
        $display(`FOO
        (
            1
            ,
            2
        ));
    end
endmodule
