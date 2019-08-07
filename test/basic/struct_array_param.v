module top;
  parameter foo_1 = { 3'b010, 2'b01, 4'b0000 };
  parameter foo_0 = { 3'b001, 2'b00, 4'b0010 };
    initial begin
        $display(foo_0);
        $display(foo_1);
    end
endmodule
