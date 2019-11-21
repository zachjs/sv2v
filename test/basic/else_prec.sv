module top;

    integer i;

    task t;
        input a, b, c;
        begin

            $display("1 (%b, %b, %b)", a, b, c);
            if (a) begin
                if (b) begin
                    $display("FOO");
                end
            end else begin
                if (c) begin
                    $display("BAR");
                end
            end

            $display("2 (%b, %b, %b)", a, b, c);
            if (a) begin
                for (i = 0; i < 1; ++i)
                if (b) begin
                    $display("FOO");
                end
            end else begin
                if (c) begin
                    $display("BAR");
                end
            end

            $display("3 (%b, %b, %b)", a, b, c);
            if (a) begin
                #1
                if (b) begin
                    $display("FOO");
                end
            end else begin
                if (c) begin
                    $display("BAR");
                end
            end

        end
    endtask

    initial begin
        t(0, 0, 0);
        t(0, 0, 1);
        t(0, 1, 0);
        t(0, 1, 1);
        t(1, 0, 0);
        t(1, 0, 1);
        t(1, 1, 0);
        t(1, 1, 1);
    end

endmodule
