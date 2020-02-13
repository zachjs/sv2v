module top;
    initial begin
        $display("FOO");
        $display("`FOO");

        $display("BAR");
        $display("`FOO");
        $display("\\BAR");
        $display("\\`FOO");

        $display("\"FOO");
        $display("\"BAR");
        $display("\"FOO\"");
        $display("\"BAR\"");

        $display("FOO");
        $display("FOOBAR");

        $display("prefixLOL");
        $display("`BAR(LOL)");
        $display("\\prefixLOL");
        $display("\\`BAR(LOL)");

        $display("prefixBAR");
        $display("`BAR(BAR)");
        $display("\\prefixBAR");
        $display("\\`BAR(BAR)");

        $display("prefixsBAR");
        $display("`BAR(sBAR)");
        $display("\\prefixsBAR");
        $display("\\`BAR(sBAR)");

        $display("FOO\"BAR");

        $display("TEST END");
        $display("TEST END");
        $display("TEST END");
        $display("TEST`FOO END");
        $display("TEST\"BAR END");
        $display("TEST\"BAR ENDBAR");
    end
endmodule
