module top;
endmodule

module dump;
    initial begin
        $display("A 3");
        $display("B 3");
        $display("C 1");
        $display("D 2");
        $display("E 2");
        $display("F 2");
        // G doesn't print
        $display("H 1");
        $display("I 1");
        $display("J 1");
        $display("K1 1");
        $display("K2 2");
        $display("K3 3");
        $display("K4 2");
        $display("K0 1");
        $display("L 1");
        $display("M1 1");
        $display("M2 1");
        $display("M3 1");
        $display("M4 1");
        $display("W::help() 1");
        $display("W::help() 1");
        $display("N1 1");
        $display("N2 2");
        $display("O1 1");
        $display("O2 2");
    end
endmodule
