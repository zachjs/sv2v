module Example;
    parameter FLAG_1 = 0;
    parameter FLAG_2 = 0;
    typedef logic [2:0] T;
    if (FLAG_1) begin
        typedef logic [1:0] T;
        T t = 0;
        initial $display("2 %b", t);
        if (FLAG_2) begin
            typedef logic [3:0] T;
            T t = 0;
            initial $display("4 %b", t);
        end
    end
    else begin
        typedef logic T;
        T t = 0;
        initial $display("1 %b", t);
    end
    T t = 0;
    initial $display("3 %b", t);
endmodule

module top;
    Example #(0, 0) a();
    Example #(1, 0) b();
    Example #(0, 1) c();
    Example #(1, 1) d();
endmodule
