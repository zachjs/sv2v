module top;
    initial begin
        $display(16);
        $display(16);
        $display(16);
        $display(3);

        $display("args %b", 2);
        $display("args %b", 2);
        $display("args %b", 2);
        $display("args %b", 2);
        $display("args %b", 2);
        $display("args %b", 2);
        $display("args %b", 1'bx);

        $display(10, 10, 16);
        $display(0, 0, 16);
        $display(9, 9, 1);
        $display(9, 9, 16);
        $display(0, 0, 1);
        $display(-1, -1, 1);
        $display(2);
        $display(1);
        $display(160);

        $display(16, 16, 1'bx);
        $display(16, 16, 1'bx);
        $display(1, 1, 1'bx);
        $display(16, 16, 1'bx);
        $display(1, 1, 1'bx);
        $display(1, 1, 1'bx);
        $display(1);
        $display(0);
        $display(16);

        $display(16, 16, 1'bx);
        $display(15, 15, 1'bx);
        $display(0, 0, 1'bx);
        $display(15, 15, 1'bx);
        $display(0, 0, 1'bx);
        $display(1, 1, 1'bx);
        $display(1);
        $display(0);
        $display(16);

        $display(16, 16, 1'bx);
        $display(15, 15, 1'bx);
        $display(0, 0, 1'bx);
        $display(15, 15, 1'bx);
        $display(0, 0, 1'bx);
        $display(1, 1, 1'bx);
        $display(1);
        $display(0);
        $display(16);

        $display(2, 2, 16);
        $display(0, 0, 16);
        $display(1, 1, 1);
        $display(1, 1, 16);
        $display(0, 0, 1);
        $display(-1, -1, 1);
        $display(2);
        $display(1);
        $display(32);

        $display(2, 2, 16);
        $display(1, 1, 16);
        $display(2, 2, 1);
        $display(2, 2, 16);
        $display(1, 1, 1);
        $display(-1, -1, 1);
        $display(2);
        $display(1);
        $display(32);

        $display(1, 1, 1'bx);
        $display(2, 2, 1'bx);
        $display(2, 2, 1'bx);
        $display(2, 2, 1'bx);
        $display(2, 2, 1'bx);
        $display(1, 1, 1'bx);
        $display(1);
        $display(0);
        $display(1);

        $display(2, 2, 10);
        $display(0, 0, 0);
        $display(1, 1, 9);
        $display(1, 1, 9);
        $display(0, 0, 0);
        $display(-1, -1, -1);
        $display(3);
        $display(2);
        $display(320);

        $display(10, 10, 16);
        $display(0, 0, 16);
        $display(9, 9, 1);
        $display(9, 9, 16);
        $display(0, 0, 1);
        $display(-1, -1, 1);
        $display(2);
        $display(1);
        $display(160);

        $display(16, 16, 1'bx);
        $display(16, 16, 1'bx);
        $display(1, 1, 1'bx);
        $display(16, 16, 1'bx);
        $display(1, 1, 1'bx);
        $display(1, 1, 1'bx);
        $display(1);
        $display(0);
        $display(16);

        $display(32, 32, 1'bx);
        $display(31, 31, 1'bx);
        $display(0, 0, 1'bx);
        $display(31, 31, 1'bx);
        $display(0, 0, 1'bx);
        $display(1, 1, 1'bx);
        $display(1);
        $display(0);
        $display(32);

        $display(1'bx, 1'bx, 1'bx);
        $display(1'bx, 1'bx, 1'bx);
        $display(1'bx, 1'bx, 1'bx);
        $display(1'bx, 1'bx, 1'bx);
        $display(1'bx, 1'bx, 1'bx);
        $display(1'bx, 1'bx, 1'bx);
        $display(0);
        $display(0);
        $display(1);

        $display(8, 8, 1'bx);
        $display(7, 7, 1'bx);
        $display(0, 0, 1'bx);
        $display(7, 7, 1'bx);
        $display(0, 0, 1'bx);
        $display(1, 1, 1'bx);
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

        $display(4, 4, 32);
        $display(0, 0, 31);
        $display(3, 3, 0);
        $display(3, 3, 31);
        $display(0, 0, 0);
        $display(-1, -1, 1);
        $display(2);
        $display(1);
        $display(128);

        $display(3, 3, 8);
        $display(0, 0, 7);
        $display(2, 2, 0);
        $display(2, 2, 7);
        $display(0, 0, 0);
        $display(-1, -1, 1);
        $display(2);
        $display(1);
        $display(24);

        $display(3, 3, 8);
        $display(0, 0, 7);
        $display(2, 2, 0);
        $display(2, 2, 7);
        $display(0, 0, 0);
        $display(-1, -1, 1);
        $display(2);
        $display(1);
        $display(24);

        $display(4, 4, 1'bx);
        $display(3, 3, 1'bx);
        $display(0, 0, 1'bx);
        $display(3, 3, 1'bx);
        $display(0, 0, 1'bx);
        $display(1, 1, 1'bx);
        $display(1);
        $display(0);
        $display(4);

        $display(4, 4, 1'bx);
        $display(3, 3, 1'bx);
        $display(0, 0, 1'bx);
        $display(3, 3, 1'bx);
        $display(0, 0, 1'bx);
        $display(1, 1, 1'bx);
        $display(1);
        $display(0);
        $display(4);

        $display(2, 2, 32);
        $display(1, 1, 31);
        $display(0, 0, 0);
        $display(1, 1, 31);
        $display(0, 0, 0);
        $display(1, 1, 1);
        $display(2);
        $display(1);
        $display(64);

        $display(2, 2, 32);
        $display(0, 0, 31);
        $display(1, 1, 0);
        $display(1, 1, 31);
        $display(0, 0, 0);
        $display(-1, -1, 1);
        $display(2);
        $display(1);
        $display(64);

    end
endmodule
