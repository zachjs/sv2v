module top;

    logic [6:0][3:0] arr;
    initial arr = 28'h9fba7d;

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
        $display("initalA: $bits(foolA):    %08d", $bits(foolA   ));
        $display("initalA: $bits(foolA[0]): %08d", $bits(foolA[0]));
        $display("initalA: foolA[0]: ", foolA[0]);
        $display("initalA: foolA[1]: ", foolA[1]);
    end
    task foolAer;
        input [1:0][1:0] foolA;
        begin
            $display("foolAer: $bits(foolA):       %08d", $bits(foolA      ));
            $display("foolAer: $bits(foolA[0]):    %08d", $bits(foolA[0]   ));
            $display("foolAer: $bits(foolA[0][0]): %08d", $bits(foolA[0][0]));
            $display("foolAer: foolA[0][0]: ", foolA[0][0]);
            $display("foolAer: foolA[0][1]: ", foolA[0][1]);
            $display("foolAer: foolA[1][0]: ", foolA[1][0]);
            $display("foolAer: foolA[1][1]: ", foolA[1][1]);
        end
    endtask


    task foolBer;
        input [1:0][1:0] foolB;
        begin
            $display("foolBer: $bits(foolB):       %08d", $bits(foolB      ));
            $display("foolBer: $bits(foolB[0]):    %08d", $bits(foolB[0]   ));
            $display("foolBer: $bits(foolB[0][0]): %08d", $bits(foolB[0][0]));
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
        $display("initalB: $bits(foolB):    %08d", $bits(foolB   ));
        $display("initalB: $bits(foolB[0]): %08d", $bits(foolB[0]));
        $display("initalB: foolB[0]: ", foolB[0]);
        $display("initalB: foolB[1]: ", foolB[1]);
    end


    initial begin
        begin : lol_block
            logic [1:0][1:0] arr;
            arr = 4'b1001;
            $display("LOL: $bits(arr):       %08d", $bits(arr      ));
            $display("LOL: $bits(arr[0]):    %08d", $bits(arr[0]   ));
            $display("LOL: $bits(arr[0][0]): %08d", $bits(arr[0][0]));
            $display("LOL: arr[0]: ", arr[0]);
            $display("LOL: arr[1]: ", arr[1]);
            $display("LOL: arr[0][1]: ", arr[0][1]);
            $display("LOL: arr[1][0]: ", arr[1][0]);
            $display("LOL: arr[1][1]: ", arr[1][1]);
        end
    end


    task magic;
    begin
        begin : magic_block1
            logic [1:0][1:0] magic_arr;
            magic_arr = 4'b1001;
            $display("MB1: $bits(magic_arr):       %08d", $bits(magic_arr      ));
            $display("MB1: $bits(magic_arr[0]):    %08d", $bits(magic_arr[0]   ));
            $display("MB1: $bits(magic_arr[0][0]): %08d", $bits(magic_arr[0][0]));
            $display("MB1: magic_arr[0]: ", magic_arr[0]);
            $display("MB1: magic_arr[1]: ", magic_arr[1]);
            $display("MB1: magic_arr[0][1]: ", magic_arr[0][1]);
            $display("MB1: magic_arr[1][0]: ", magic_arr[1][0]);
            $display("MB1: magic_arr[1][1]: ", magic_arr[1][1]);
        end
        begin : magic_block2
            logic [3:0] magic_arr;
            magic_arr = 4'b1001;
            $display("MB2: $bits(magic_arr):       %08d", $bits(magic_arr      ));
            $display("MB2: $bits(magic_arr[0]):    %08d", $bits(magic_arr[0]   ));
            $display("MB2: magic_arr[0]: ", magic_arr[0]);
            $display("MB2: magic_arr[1]: ", magic_arr[1]);
            $display("MB2: magic_arr[2]: ", magic_arr[2]);
            $display("MB2: magic_arr[3]: ", magic_arr[3]);
        end
        begin : magic_block3
            logic [1:0][1:0][1:0] magic_arr;
            magic_arr = 4'b1001;
            $display("MB3: $bits(magic_arr):          %08d", $bits(magic_arr         ));
            $display("MB3: $bits(magic_arr[0]):       %08d", $bits(magic_arr[0]      ));
            $display("MB3: $bits(magic_arr[0][0]):    %08d", $bits(magic_arr[0][0]   ));
            $display("MB3: $bits(magic_arr[0][0][0]): %08d", $bits(magic_arr[0][0][0]));
            $display("MB3: magic_arr[0]: ", magic_arr[0]);
            $display("MB3: magic_arr[1]: ", magic_arr[1]);
            $display("MB3: magic_arr[0][0]: ", magic_arr[0][0]);
            $display("MB3: magic_arr[0][1]: ", magic_arr[0][1]);
            $display("MB3: magic_arr[1][0]: ", magic_arr[1][0]);
            $display("MB3: magic_arr[1][1]: ", magic_arr[1][1]);
            $display("MB3: magic_arr[0][0][0]: ", magic_arr[0][0][0]);
            $display("MB3: magic_arr[0][0][1]: ", magic_arr[0][0][1]);
            $display("MB3: magic_arr[0][1][0]: ", magic_arr[0][1][0]);
            $display("MB3: magic_arr[0][1][1]: ", magic_arr[0][1][1]);
            $display("MB3: magic_arr[1][0][1]: ", magic_arr[1][0][1]);
            $display("MB3: magic_arr[1][0][0]: ", magic_arr[1][0][0]);
            $display("MB3: magic_arr[1][1][1]: ", magic_arr[1][1][1]);
            $display("MB3: magic_arr[1][1][0]: ", magic_arr[1][1][0]);
            begin : magic_block4
                logic [1:0][1:0] magic_arr;
                magic_arr = 4'b1001;
                $display("MB4: $bits(magic_arr):       %08d", $bits(magic_arr      ));
                $display("MB4: $bits(magic_arr[0]):    %08d", $bits(magic_arr[0]   ));
                $display("MB4: $bits(magic_arr[0][0]): %08d", $bits(magic_arr[0][0]));
                $display("MB4: magic_arr[0]: ", magic_arr[0]);
                $display("MB4: magic_arr[1]: ", magic_arr[1]);
                $display("MB4: magic_arr[0][1]: ", magic_arr[0][1]);
                $display("MB4: magic_arr[1][0]: ", magic_arr[1][0]);
                $display("MB4: magic_arr[1][1]: ", magic_arr[1][1]);
            end
            magic_arr = 4'b1001;
            $display("MB3: $bits(magic_arr):          %08d", $bits(magic_arr         ));
            $display("MB3: $bits(magic_arr[0]):       %08d", $bits(magic_arr[0]      ));
            $display("MB3: $bits(magic_arr[0][0]):    %08d", $bits(magic_arr[0][0]   ));
            $display("MB3: $bits(magic_arr[0][0][0]): %08d", $bits(magic_arr[0][0][0]));
            $display("MB3: magic_arr[0]: ", magic_arr[0]);
            $display("MB3: magic_arr[1]: ", magic_arr[1]);
            $display("MB3: magic_arr[0][0]: ", magic_arr[0][0]);
            $display("MB3: magic_arr[0][1]: ", magic_arr[0][1]);
            $display("MB3: magic_arr[1][0]: ", magic_arr[1][0]);
            $display("MB3: magic_arr[1][1]: ", magic_arr[1][1]);
            $display("MB3: magic_arr[0][0][0]: ", magic_arr[0][0][0]);
            $display("MB3: magic_arr[0][0][1]: ", magic_arr[0][0][1]);
            $display("MB3: magic_arr[0][1][0]: ", magic_arr[0][1][0]);
            $display("MB3: magic_arr[0][1][1]: ", magic_arr[0][1][1]);
            $display("MB3: magic_arr[1][0][1]: ", magic_arr[1][0][1]);
            $display("MB3: magic_arr[1][0][0]: ", magic_arr[1][0][0]);
            $display("MB3: magic_arr[1][1][1]: ", magic_arr[1][1][1]);
            $display("MB3: magic_arr[1][1][0]: ", magic_arr[1][1][0]);
        end
    end
    endtask
    initial magic();


    initial begin
        begin : ntf_magic_block1
            logic [1:0][1:0] ntf_magic_arr;
            ntf_magic_arr = 4'b1001;
            $display("NTFMB1: $bits(ntf_magic_arr):       %08d", $bits(ntf_magic_arr      ));
            $display("NTFMB1: $bits(ntf_magic_arr[0]):    %08d", $bits(ntf_magic_arr[0]   ));
            $display("NTFMB1: $bits(ntf_magic_arr[0][0]): %08d", $bits(ntf_magic_arr[0][0]));
            $display("NTFMB1: ntf_magic_arr[0]: ", ntf_magic_arr[0]);
            $display("NTFMB1: ntf_magic_arr[1]: ", ntf_magic_arr[1]);
            $display("NTFMB1: ntf_magic_arr[0][1]: ", ntf_magic_arr[0][1]);
            $display("NTFMB1: ntf_magic_arr[1][0]: ", ntf_magic_arr[1][0]);
            $display("NTFMB1: ntf_magic_arr[1][1]: ", ntf_magic_arr[1][1]);
        end
        begin : ntf_magic_block2
            logic [3:0] ntf_magic_arr;
            ntf_magic_arr = 4'b1001;
            $display("NTFMB2: $bits(ntf_magic_arr):       %08d", $bits(ntf_magic_arr      ));
            $display("NTFMB2: $bits(ntf_magic_arr[0]):    %08d", $bits(ntf_magic_arr[0]   ));
            $display("NTFMB2: ntf_magic_arr[0]: ", ntf_magic_arr[0]);
            $display("NTFMB2: ntf_magic_arr[1]: ", ntf_magic_arr[1]);
            $display("NTFMB2: ntf_magic_arr[2]: ", ntf_magic_arr[2]);
            $display("NTFMB2: ntf_magic_arr[3]: ", ntf_magic_arr[3]);
        end
        begin : ntf_magic_block3
            logic [1:0][1:0][1:0] ntf_magic_arr;
            ntf_magic_arr = 4'b1001;
            $display("NTFMB3: $bits(ntf_magic_arr):          %08d", $bits(ntf_magic_arr         ));
            $display("NTFMB3: $bits(ntf_magic_arr[0]):       %08d", $bits(ntf_magic_arr[0]      ));
            $display("NTFMB3: $bits(ntf_magic_arr[0][0]):    %08d", $bits(ntf_magic_arr[0][0]   ));
            $display("NTFMB3: $bits(ntf_magic_arr[0][0][0]): %08d", $bits(ntf_magic_arr[0][0][0]));
            $display("NTFMB3: ntf_magic_arr[0]: ", ntf_magic_arr[0]);
            $display("NTFMB3: ntf_magic_arr[1]: ", ntf_magic_arr[1]);
            $display("NTFMB3: ntf_magic_arr[0][0]: ", ntf_magic_arr[0][0]);
            $display("NTFMB3: ntf_magic_arr[0][1]: ", ntf_magic_arr[0][1]);
            $display("NTFMB3: ntf_magic_arr[1][0]: ", ntf_magic_arr[1][0]);
            $display("NTFMB3: ntf_magic_arr[1][1]: ", ntf_magic_arr[1][1]);
            $display("NTFMB3: ntf_magic_arr[0][0][0]: ", ntf_magic_arr[0][0][0]);
            $display("NTFMB3: ntf_magic_arr[0][0][1]: ", ntf_magic_arr[0][0][1]);
            $display("NTFMB3: ntf_magic_arr[0][1][0]: ", ntf_magic_arr[0][1][0]);
            $display("NTFMB3: ntf_magic_arr[0][1][1]: ", ntf_magic_arr[0][1][1]);
            $display("NTFMB3: ntf_magic_arr[1][0][1]: ", ntf_magic_arr[1][0][1]);
            $display("NTFMB3: ntf_magic_arr[1][0][0]: ", ntf_magic_arr[1][0][0]);
            $display("NTFMB3: ntf_magic_arr[1][1][1]: ", ntf_magic_arr[1][1][1]);
            $display("NTFMB3: ntf_magic_arr[1][1][0]: ", ntf_magic_arr[1][1][0]);
            begin : ntf_magic_block4
                logic [1:0][1:0] ntf_magic_arr;
                ntf_magic_arr = 4'b1001;
                $display("NTFMB4: $bits(ntf_magic_arr):       %08d", $bits(ntf_magic_arr      ));
                $display("NTFMB4: $bits(ntf_magic_arr[0]):    %08d", $bits(ntf_magic_arr[0]   ));
                $display("NTFMB4: $bits(ntf_magic_arr[0][0]): %08d", $bits(ntf_magic_arr[0][0]));
                $display("NTFMB4: ntf_magic_arr[0]: ", ntf_magic_arr[0]);
                $display("NTFMB4: ntf_magic_arr[1]: ", ntf_magic_arr[1]);
                $display("NTFMB4: ntf_magic_arr[0][1]: ", ntf_magic_arr[0][1]);
                $display("NTFMB4: ntf_magic_arr[1][0]: ", ntf_magic_arr[1][0]);
                $display("NTFMB4: ntf_magic_arr[1][1]: ", ntf_magic_arr[1][1]);
            end
            ntf_magic_arr = 4'b1001;
            $display("NTFMB3: $bits(ntf_magic_arr):          %08d", $bits(ntf_magic_arr         ));
            $display("NTFMB3: $bits(ntf_magic_arr[0]):       %08d", $bits(ntf_magic_arr[0]      ));
            $display("NTFMB3: $bits(ntf_magic_arr[0][0]):    %08d", $bits(ntf_magic_arr[0][0]   ));
            $display("NTFMB3: $bits(ntf_magic_arr[0][0][0]): %08d", $bits(ntf_magic_arr[0][0][0]));
            $display("NTFMB3: ntf_magic_arr[0]: ", ntf_magic_arr[0]);
            $display("NTFMB3: ntf_magic_arr[1]: ", ntf_magic_arr[1]);
            $display("NTFMB3: ntf_magic_arr[0][0]: ", ntf_magic_arr[0][0]);
            $display("NTFMB3: ntf_magic_arr[0][1]: ", ntf_magic_arr[0][1]);
            $display("NTFMB3: ntf_magic_arr[1][0]: ", ntf_magic_arr[1][0]);
            $display("NTFMB3: ntf_magic_arr[1][1]: ", ntf_magic_arr[1][1]);
            $display("NTFMB3: ntf_magic_arr[0][0][0]: ", ntf_magic_arr[0][0][0]);
            $display("NTFMB3: ntf_magic_arr[0][0][1]: ", ntf_magic_arr[0][0][1]);
            $display("NTFMB3: ntf_magic_arr[0][1][0]: ", ntf_magic_arr[0][1][0]);
            $display("NTFMB3: ntf_magic_arr[0][1][1]: ", ntf_magic_arr[0][1][1]);
            $display("NTFMB3: ntf_magic_arr[1][0][1]: ", ntf_magic_arr[1][0][1]);
            $display("NTFMB3: ntf_magic_arr[1][0][0]: ", ntf_magic_arr[1][0][0]);
            $display("NTFMB3: ntf_magic_arr[1][1][1]: ", ntf_magic_arr[1][1][1]);
            $display("NTFMB3: ntf_magic_arr[1][1][0]: ", ntf_magic_arr[1][1][0]);
        end
    end


    function [0:1][0:1] ret;
        input unused;
        ret = 4'b1010;
    endfunction
    logic [3:0] ret_res;
    initial begin
        ret_res = ret(0);
        $display("ret: ", ret_res);
        $display("ret[0]: ", ret_res[0]);
        $display("ret[1]: ", ret_res[1]);
        $display("ret[2]: ", ret_res[2]);
        $display("ret[3]: ", ret_res[3]);
    end

endmodule
