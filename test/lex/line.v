module top;
    initial begin
        $display("line.sv", `__LINE__);
        $display("fake.v", 102);
        $display("via include: ", "./line.vh", 1);
        $display("line.vh", 201);
    end
endmodule
