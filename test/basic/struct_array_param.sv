typedef struct packed {
    logic [2:0] a;
    logic [1:0] b;
    logic [3:0] c;
} foo_s;

parameter foo_s [1:0] foo = {
    '{ a: 2, b: 1, c: 0 },
    '{ a: 1, b: 0, c: 2 }
};

module top;
    initial begin
        $display(foo[0]);
        $display(foo[1]);
    end
endmodule
