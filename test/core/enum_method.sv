module top;
    localparam type T = enum integer unsigned {
        A = 5,
        B[3],
        C,
        D = 100,
        E
    };
    T x = x.first;
    initial begin
        T y;
        localparam A = 0, B = 0, B0 = 0, B1 = 0, B2 = 0, C = 0, D = 0, E = 0;
        $display("first = %0d", x.first);
        $display("last = %0d", x.last);
        $display("num = %0d", x.num);

        while (1) begin
            $display("%2s = %0d", x.name, x);
            if (x == x.last)
                break;
            x = x.next;
        end

        while (1) begin
            $display("%2s = %0d", x.name(), x);
            if (x == x.first())
                break;
            x = x.prev();
        end

        while (1) begin
            $display("%2s = %0d", x.name, x);
            if (x == x.last.prev)
                break;
            x = x.next(2);
        end

        $display("name = %2s", y.name);
        $display("next = %0d", y.next);
        $display("prev = %0d", y.prev);
        $display("next(2) = %0d", y.next(2));
        $display("prev(2) = %0d", y.prev(2));
    end

    localparam type U = enum byte {
        X, Y, Z
    };
    initial begin
        localparam X = 0, Y = 0, Z = 0;
        localparam U x = x.first;
        localparam U y [2] = '{y[0].first, y[1].last};

        $display("first = %b %b", x.first, y[0].first);
        $display("last = %b %b", x.last, y[0].last);
        $display("num = %b %b", x.num, y[0].num);

        for (integer i = -3; i <= 3; i += 1) begin
            $display("next(%0d) = %b %b", i, x.next(i), y[0].next(i));
            $display("prev(%0d) = %b %b", i, x.prev(i), y[0].prev(i));
        end
        $display("next(%0d) = %b %b", 4, x.next(.N(4)), y[0].next(.N(4)));
        $display("prev(%0d) = %b %b", 4, x.prev(.N(4)), y[0].prev(.N(4)));
    end
endmodule
