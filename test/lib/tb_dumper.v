module sv2v_dumper;
    initial begin
        $dumpfile(`TEST_VCD);
        $dumpvars(1, top);
    end
endmodule
