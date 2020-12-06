interface Interface;
    logic x;
endinterface

module top;
    Interface intfs[3:2][8:5]();
    for (genvar x = 2; x <= 3; x = x + 1)
        for (genvar y = 5; y <= 8; y = y + 1)
            assign intfs[x][y].x = '1;
endmodule
