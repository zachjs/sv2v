module top;
    initial begin
        $display("%b",
            5
            	/* intentional tab */
            'b
            	/* intentional tab */
            01010
        );
        $display("%b",
            3
            	/* intentional tab */
            'o
            	/* intentional tab */
            7
        );
        $display("%b",
            8
            	/* intentional tab */
            'h
            	/* intentional tab */
            ab
        );
        $display("%b",
            8
            	/* intentional tab */
            'd
            	/* intentional tab */
            11
        );
        $display("%b",
            8
            	/* intentional tab */
            'd
            	/* intentional tab */
            x___
        );
        $display("%b",
            8
            	/* intentional tab */
            'd
            	/* intentional tab */
            z___
        );
    end
endmodule
