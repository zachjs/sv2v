module top;
    initial begin
        $display("line.sv", `__LINE__);
        ;
        $display("fake.v", 102);
    end
endmodule
