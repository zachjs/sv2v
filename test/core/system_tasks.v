module top;
	initial begin
		$write("[%0t] Info: ", $time);
		$display;
		$write("[%0t] Info: ", $time);
		$display("%b", 1);
		$write("[%0t] Warning: ", $time);
		$display;
		$write("[%0t] Warning: ", $time);
		$display("%b", 2);
		$write("[%0t] Error: ", $time);
		$display;
		$write("[%0t] Error: ", $time);
		$display("%b", 3);
		$display("[%0t] Fatal:", $time);
		$finish;
		$write("[%0t] Fatal: ", $time);
		$display;
		$finish(0);
		$write("[%0t] Fatal: ", $time);
		$display("%b", 4);
		$finish(0);
	end
endmodule
