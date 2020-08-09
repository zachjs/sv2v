module top;
    generate
        begin : x
            integer x;
            function z;
                input x;
                z = ~x;
            endfunction
        end
    endgenerate
    initial x.x = 1;
    generate
        begin : y
            function z;
                input x;
                z = x;
            endfunction
            integer x = 0;
            initial begin
                #1;
                $display("x = %b", x);
                $display("z(x) = %b", z(x));
                $display("y.x = %b", top.x.x);
                $display("y.z(x) = %b", top.x.z(x));
                $display("y.z(y.x) = %b", top.x.z(top.x.x));
                $display("y.z(z(y.x)) = %b", top.x.z(z(top.x.x)));
            end
        end
    endgenerate
endmodule
