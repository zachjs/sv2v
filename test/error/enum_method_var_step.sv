// pattern: cannot convert enum method next/prev
// location: enum_method_var_step.sv:9:9
module top;
    typedef enum { A, B, C } e_t;
    e_t x;
    integer k;
    initial begin
        k = 2;
        $display("%0d", x.next(k));
    end
endmodule
