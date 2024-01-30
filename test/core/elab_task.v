module top;
    integer _sv2v_elaboration_fatal = -1;
    initial $display("Elaboration Info: ");
    initial $display("Elaboration Info: ", "%b", 1);
    initial $display("Elaboration Warning: ");
    initial $display("Elaboration Warning: ", "%b", 2);
    initial $display("Elaboration Error: ");
    initial $display("Elaboration Error: ", "%b", 3);
    initial begin
        $display("Elaboration Fatal: ");
        _sv2v_elaboration_fatal = 0;
    end
    initial begin
        $display("Elaboration Fatal: ");
        _sv2v_elaboration_fatal = 0;
    end
    initial begin
        $display("Elaboration Fatal: ", "%b", 4);
        _sv2v_elaboration_fatal = 0;
    end
    initial begin
        #0;
        if (_sv2v_elaboration_fatal != -1)
            $finish(_sv2v_elaboration_fatal);
    end
endmodule
