// Enumerated type methods (IEEE 1800-2017 6.19.5)
module top;
    typedef enum logic [1:0] {
        A = 2'b00,
        B = 2'b01,
        C = 2'b10,
        D = 2'b11
    } e_t;
    typedef struct packed {
        e_t vsew;
    } s_t;

    s_t q;
    e_t r, s;
    integer n;

    initial begin
        q = 2'b01; // B
        r = q.vsew.next();
        s = q.vsew.prev();
        n = q.vsew.num();
        $display("next=%0d prev=%0d first=%0d last=%0d num=%0d",
            r, s, q.vsew.first(), q.vsew.last(), n);
        q = 2'b11; // D
        r = q.vsew.next();
        s = q.vsew.prev(2);
        $display("wrap_next=%0d prev2=%0d", r, s);
    end
endmodule
