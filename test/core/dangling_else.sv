`define TESTA(n, X) \
	always @* \
		if (a[n]) \
			X if (b[n]) \
				c[n] = 1; \
			else \
				c[n] = 0;
`define TESTC(n, X) \
	always @* \
		if (a[n]) begin \
			X if (b[n]) \
				c[n] = 1; \
		end else \
			c[n] = 0;
`define NOTHING
`define ATTRIBUTE (* test *)
`define FOR for (i = 0; i < 1; i = i + 1)
`define WHILE while (i < 1)
`define REPEAT repeat (i)
`define FOREACH foreach (a[j])
`define FOREVER forever #10
`define TIMING #10
module top();
	wire [15:0] a, b;
	reg [15:0] c;
	integer i;
	`TESTA(0, `NOTHING)
	`TESTC(1, `NOTHING)
	`TESTA(2, `ATTRIBUTE)
	`TESTC(3, `ATTRIBUTE)
	`TESTA(4, `FOR)
	`TESTC(5, `FOR)
	`TESTA(6, `WHILE)
	`TESTC(7, `WHILE)
	`TESTA(8, `REPEAT)
	`TESTC(9, `REPEAT)
	`TESTA(10, `FOREACH)
	`TESTC(11, `FOREACH)
	`TESTA(12, `FOREVER)
	`TESTC(13, `FOREVER)
	`TESTA(14, `TIMING)
	`TESTC(15, `TIMING)
endmodule
