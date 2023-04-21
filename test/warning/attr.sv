module mod(output logic out);
    assign out = 1;
endmodule
module top;
    logic x;
    mod m((* foo *)(* bar *).out(x));
endmodule
