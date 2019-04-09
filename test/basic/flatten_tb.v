`define FOO(tag) \
    wire [14:0] tag``one_out, tag``two_out, tag``thr_out, tag``fou_out; \
    tag``1 tag``one(.clock(clock), .in(in), .out(tag``one_out)); \
    tag``2 tag``two(.clock(clock), .in(in), .out(tag``two_out)); \
    tag``3 tag``thr(.clock(clock), .in(in), .out(tag``thr_out)); \
    tag``4 tag``fou(.clock(clock), .in(in), .out(tag``fou_out)); \
    integer tag``i; \
    initial begin \
        for (tag``i = 0; tag``i < 20; tag``i++) begin \
            #2; \
            $display(`"tag", $time, ": %h %15b %15b %15b %15b", in, \
                tag``one_out, tag``two_out, tag``thr_out, tag``fou_out); \
        end \
    end

module top;
    reg clock, in;

    initial begin
        clock = 1;
        forever #1 clock = ~clock;
    end

    integer i;
    localparam [20:0] pattern = 20'b01101100001010101100;
    initial begin
        for (i = 0; i < 20; i++) begin
            in = pattern[i];
            #2;
        end
        $finish;
    end

    `FOO(A)
    `FOO(B)
    `FOO(C)
    `FOO(D)

endmodule
