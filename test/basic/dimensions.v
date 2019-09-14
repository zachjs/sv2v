module top;
    initial begin
        $display(16);
        $display(16);
        $display(16);
        $display(3);

        $display(10, 10, 16);
        $display(0, 0, 16);
        $display(9, 9, 1);
        $display(9, 9, 16);
        $display(0, 0, 1);
        $display(-1, -1, 1);
        $display(2);
        $display(1);
        $display(160);

        $display(16, 16, 'bx);
        $display(16, 16, 'bx);
        $display(1, 1, 'bx);
        $display(16, 16, 'bx);
        $display(1, 1, 'bx);
        $display(1, 1, 'bx);
        $display(1);
        $display(0);
        $display(16);

        $display(32, 32, 'bx);
        $display(31, 31, 'bx);
        $display(0, 0, 'bx);
        $display(31, 31, 'bx);
        $display(0, 0, 'bx);
        $display(1, 1, 'bx);
        $display(1);
        $display(0);
        $display(32);

        $display('bx, 'bx, 'bx);
        $display('bx, 'bx, 'bx);
        $display('bx, 'bx, 'bx);
        $display('bx, 'bx, 'bx);
        $display('bx, 'bx, 'bx);
        $display('bx, 'bx, 'bx);
        $display(0);
        $display(0);
        $display(1);

        $display(8, 8, 'bx);
        $display(7, 7, 'bx);
        $display(0, 0, 'bx);
        $display(7, 7, 'bx);
        $display(0, 0, 'bx);
        $display(1, 1, 'bx);
        $display(1);
        $display(0);
        $display(8);

        $display(4, 4, 32);
        $display(3, 3, 31);
        $display(0, 0, 0);
        $display(3, 3, 31);
        $display(0, 0, 0);
        $display(1, 1, 1);
        $display(2);
        $display(1);
        $display(128);
    end
endmodule
