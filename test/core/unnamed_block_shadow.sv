`define TEST \
    reg x; \
    begin \
        reg [1:0] x; \
        $display("%0d %b", $bits(x), x); \
    end \
    $display("%0d %b", $bits(x), x);

module top;
    task t;
        input integer unused;
        `TEST
    endtask
    function f;
        input integer unused;
        `TEST
    endfunction
    initial t(f(0));
    initial begin
        `TEST
    end
endmodule
