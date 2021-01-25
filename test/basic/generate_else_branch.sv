module Example;
    parameter X = 1;
    parameter Y = 1;
    parameter Z = 1;
    initial $display("X=%0d Y=%0d Z=%0d Example", X, Y, Z);
    generate
        if (X) begin
            if (Y) begin
                if (Z) begin
                    initial $display("X=%0d Y=%0d Z=%0d A", X, Y, Z);
                end
            end
            else begin
                initial $display("X=%0d Y=%0d Z=%0d B", X, Y, Z);
            end
        end
        initial
            if (X) begin
                if (Y) begin
                    if (Z) begin
                        $display("X=%0d Y=%0d Z=%0d C", X, Y, Z);
                    end
                end
                else begin
                    $display("X=%0d Y=%0d Z=%0d D", X, Y, Z);
                end
            end
        if (X) begin
            initial
                if (Y) begin
                    if (Z) begin
                        $display("X=%0d Y=%0d Z=%0d E", X, Y, Z);
                    end
                end
                else begin
                    $display("X=%0d Y=%0d Z=%0d F", X, Y, Z);
                end
        end
        if (X) begin
            if (Y) begin
                initial
                    if (Z) begin
                        $display("X=%0d Y=%0d Z=%0d G", X, Y, Z);
                    end
            end
            else begin
                initial $display("X=%0d Y=%0d Z=%0d H", X, Y, Z);
            end
        end
    endgenerate
endmodule

module top;
    Example #(0, 0, 0) e000();
    Example #(0, 0, 1) e001();
    Example #(0, 1, 0) e010();
    Example #(0, 1, 1) e011();
    Example #(1, 0, 0) e100();
    Example #(1, 0, 1) e101();
    Example #(1, 1, 0) e110();
    Example #(1, 1, 1) e111();
endmodule
