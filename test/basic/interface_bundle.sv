interface bundle;
    logic [1:0] index;
    logic clock;
    logic [3:0] inp;
    logic out;
endinterface

module rotator(bundle b);
    initial b.index = 0;
    always @(posedge b.clock)
        b.index <= b.index + 1;
endmodule

module setter(bundle b);
    initial b.inp = '1;
    always @(posedge b.clock)
        b.inp[b.index] <= b.out;
endmodule

module reducer(bundle b);
    assign b.out = ^b.inp;
endmodule

module clocker(bundle b);
    initial begin
        b.clock <= 0;
        forever
            #5 b.clock <= ~b.clock;
    end
endmodule

module top;
    bundle b();
    rotator rot(b);
    setter set(b);
    reducer red(b);
    clocker clk(b);
    initial begin
        $monitor("%b %b %b %b", b.index, b.clock, b.inp, b.out);
        #100;
        $finish;
    end
endmodule
