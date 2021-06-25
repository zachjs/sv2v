module top;
    reg data;
    initial #1 $display("Interface %b", data);
    initial #2 $display("Module %b", data);
    initial begin
        data = 0;
        #1;
        data = 1;
        #1;
    end
endmodule
