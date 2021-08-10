`define DUMP(L) initial $display(`"L %0d`", $bits(i));
module top;
    genvar i;
    for (i = 0; i < 1; i = 1) begin
        `DUMP(A)
        if (1) begin
            localparam [9:0] i = 1;
            `DUMP(B)
            if (1) begin
                genvar i;
                for (i = 0; i < 1; i = 1) begin
                    `DUMP(C)
                    if (1) begin
                        localparam [5:0] i = 1;
                        `DUMP(D)
                        if (1) begin
                            genvar i;
                            for (i = 0; i < 1; i = 1)
                                `DUMP(E)
                        end
                    end
                end
            end
        end
    end
endmodule
