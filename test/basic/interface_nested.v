module top;
    wire x, f_x;
    wire f_a_x, f_b_x;
    assign x = 1;
    assign f_x = x;
    assign f_a_x = x;
    assign f_b_x = ~x;

    initial begin
        $display("bar got %b", f_a_x);
        $display("bar got %b", f_b_x);
    end
endmodule
