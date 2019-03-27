module sv2v_dumper;
    initial begin
        $dumpfile(`TEST_VCD);
        $dumpvars(1, `TEST_TOP);
    end
endmodule
