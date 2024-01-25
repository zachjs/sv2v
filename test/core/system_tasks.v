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
		$write("[%0t] Fatal: ", $time);
		$display;
		$finish;
		$write("Fatal:");
		$display("%b", 4);
		$finish(0);
	end
endmodule
