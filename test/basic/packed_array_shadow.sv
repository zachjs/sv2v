module top;

    function parity;
        input [3:0] arr;
        parity = arr[2] ^ arr[2] ^ arr[1] ^ arr[0];
    endfunction

    task loop;
        input [3:0] arr;
        begin
            arr = 4'b0000;
            repeat (2**4) begin
                $display("%04b -> %d", arr, parity(arr));
                arr += 1;
            end
        end
    endtask

    task dump;
        begin : dump_block
            integer i;
            for (i = 0; i < 7; i += 1) begin
                $display("arr[%1d] = %04b", i, arr[i]);
            end
        end
    endtask

    task reinterpret;
        input [3:0][6:0] arr;
        begin : reinterpret_block
            integer i;
            for (i = 0; i < 4; i += 1) begin
                $display("arr'[%1d] = %07b", i, arr[i]);
            end
        end
    endtask

    logic [6:0][3:0] arr;
    initial arr = 28'h9fba7d;

    integer i;
    initial begin
        for (i = 0; i < 7; i += 1) begin
            loop(arr[i]);
            dump();
            reinterpret(arr);
        end
    end


    logic [1:0] foolA;
    initial begin
        foolA = 2'b10;
        foolAer(4'b1001);
        $display("initalA: $bits(foolA):    ", $bits(foolA   ));
        $display("initalA: $bits(foolA[0]): ", $bits(foolA[0]));
        $display("initalA: foolA[0]: ", foolA[0]);
        $display("initalA: foolA[1]: ", foolA[1]);
    end
    task foolAer;
        input [1:0][1:0] foolA;
        begin
            $display("foolAer: $bits(foolA):       ", $bits(foolA      ));
            $display("foolAer: $bits(foolA[0]):    ", $bits(foolA[0]   ));
            $display("foolAer: $bits(foolA[0][0]): ", $bits(foolA[0][0]));
            $display("foolAer: foolA[0][0]: ", foolA[0][0]);
            $display("foolAer: foolA[0][1]: ", foolA[0][1]);
            $display("foolAer: foolA[1][0]: ", foolA[1][0]);
            $display("foolAer: foolA[1][1]: ", foolA[1][1]);
        end
    endtask


    task foolBer;
        input [1:0][1:0] foolB;
        begin
            $display("foolBer: $bits(foolB):       ", $bits(foolB      ));
            $display("foolBer: $bits(foolB[0]):    ", $bits(foolB[0]   ));
            $display("foolBer: $bits(foolB[0][0]): ", $bits(foolB[0][0]));
            $display("foolBer: foolB[0][0]: ", foolB[0][0]);
            $display("foolBer: foolB[0][1]: ", foolB[0][1]);
            $display("foolBer: foolB[1][0]: ", foolB[1][0]);
            $display("foolBer: foolB[1][1]: ", foolB[1][1]);
        end
    endtask
    logic [1:0] foolB;
    initial begin
        foolB = 2'b10;
        foolBer(4'b1001);
        $display("initalB: $bits(foolB):    ", $bits(foolB   ));
        $display("initalB: $bits(foolB[0]): ", $bits(foolB[0]));
        $display("initalB: foolB[0]: ", foolB[0]);
        $display("initalB: foolB[1]: ", foolB[1]);
    end


    task magic;
    begin
        begin : magic_block
            logic [1:0][1:0] magic_arr;
            magic_arr = 4'b1001;
            $display("$bits(magic_arr):       ", $bits(magic_arr      ));
            $display("$bits(magic_arr[0]):    ", $bits(magic_arr[0]   ));
            $display("$bits(magic_arr[0][0]): ", $bits(magic_arr[0][0]));
            $display("magic_arr[0][0]: ", magic_arr[0][0]);
            $display("magic_arr[0][1]: ", magic_arr[0][1]);
            $display("magic_arr[1][0]: ", magic_arr[1][0]);
            $display("magic_arr[1][1]: ", magic_arr[1][1]);
        end
    end
    endtask
    initial magic();

endmodule
