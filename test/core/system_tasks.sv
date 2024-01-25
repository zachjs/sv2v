module top;
    initial begin
        $info;
        $info("%b", 1);
        $warning;
        $warning("%b", 2);
        $error;
        $error("%b", 3);
        $fatal;
        $fatal(0, "%b", 4);
    end
endmodule
