module top;

    initial begin
        logic x;
        $display($bits(x));
        begin
            logic [0:$bits(x)] x;
            $display($bits(x));
            begin
                logic [0:$bits(x)] x;
                $display($bits(x));
            end
        end
    end

    initial begin
        logic x;
        $display($bits(type(x)));
        begin
            logic [0:$bits(type(x))] x;
            $display($bits(type(x)));
            begin
                logic [0:$bits(type(x))] x;
                $display($bits(type(x)));
            end
        end
    end

    initial begin
        logic x;
        $display($bits(x));
        begin
            logic [0:$bits(type(x))] x;
            $display($bits(x));
            begin
                logic [0:$bits(type(x))] x;
                $display($bits(x));
            end
        end
    end

    initial begin
        logic x;
        $display($bits(type(x)));
        begin
            logic [0:$bits(x)] x;
            $display($bits(type(x)));
            begin
                logic [0:$bits(x)] x;
                $display($bits(type(x)));
            end
        end
    end

endmodule
