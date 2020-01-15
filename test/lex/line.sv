module top;
    initial begin
        $display(`__FILE__, `__LINE__);
`line 101 "fake.v" 1
        $display(`__FILE__, `__LINE__);
`define foo(filename) `"filename.vh`"
`include `foo(line)
`define new_line_num 200
`line `new_line_num `foo(line) 1
        $display(`__FILE__, `__LINE__);
    end
endmodule
