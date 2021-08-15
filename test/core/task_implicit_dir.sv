module top;
    task t(
        integer inp,
        output byte out1,
        shortint out2
    );
        $display("t(inp = %0d)", inp);
        out1 = inp;
        out2 = inp;
    endtask
    initial begin
        integer a;
        byte b;
        shortint c;
        a = 5;
        t(a, b, c);
        $display("a = %0d, b = %0d, c = %0d", a, b, c);
    end
endmodule
