module top;
    localparam type T = logic;
    initial begin
        $display("A %0d", $bits(T));
        begin
            localparam type T = logic [1:0];
            $display("B %0d", $bits(T));
            begin
                localparam type T = T [1:0];
                $display("C %0d", $bits(T));
            end
            $display("B %0d", $bits(T));
        end
        $display("A %0d", $bits(T));
    end
endmodule
