initial begin
    $display("via include: ", `__FILE__, `__LINE__);
end
`define EXTRA_CASE \
    initial begin \
        $display("via included macro: ", `__FILE__, `__LINE__); \
    end
