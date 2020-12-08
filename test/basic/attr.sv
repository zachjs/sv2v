module Example(
    input a, b,
    (* who=1 *) output c,
    (* what=2 *) output d, e,
    input f,
    (* when=3 *) (* where=4 *) output g, h, i
);
endmodule

(* a=1 *) module top;
    (* foo="bar" *) reg x;
    initial begin
        x = 1;
        $display(x);
    end

    reg a, b;
    wire c;
    wire d, e;
    reg f;
    wire g, h, i;
    Example m(a,b,c,d,e,f,g,h,i);
endmodule
