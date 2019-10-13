module test;
    typedef struct packed {
        int w, x;
        byte y;
        logic z;
    } struct_a;
    struct_a a;
    initial begin
        $monitor("%2d: %b %b %b %b %b", $time, a, a.w, a.x, a.y, a.z);

        #1 a.w = 0;
        #1 a.x = 0;
        #1 a.y = 0;
        #1 a.z = 0;

        #1 a = '{default: 1};
        #1 a = '{default: 2};
        #1 a = '{default: 3};
        #1 a = '{default: 0};
        #1 a = '{default: -1};
        #1 a = '{default: -2};

        #1 a = '{int: 0, default: 1};
        #1 a = '{byte: 0, default: 1};
        #1 a = '{logic: 0, default: 1};
        #1 a = '{logic: 1, int: 2, byte: 3};
        #1 a = '{logic: 1, int: 2, byte: 3, default: -1};
        #1 a = '{int: 3, byte: 2, default: 0};

        #1 a = '{w: 8, int: 0, default: 1};
        #1 a = '{w: 8, byte: 0, default: 1};
        #1 a = '{w: 8, logic: 0, default: 1};
        #1 a = '{w: 8, logic: 1, int: 2, byte: 3};
        #1 a = '{w: 8, logic: 1, int: 2, byte: 3, default: -1};
        #1 a = '{w: 8, int: 3, byte: 2, default: 0};

    end
endmodule

module top; endmodule
