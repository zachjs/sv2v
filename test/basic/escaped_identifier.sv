module mod(
    input \AFancySignalName[3].Something ,
    output \AFancySignalName[3].SomethingElse
);
endmodule

module top;
    wire \BFancySignalName.Something = 1;
    wire \BFancySignalName.SomethingElse ;
    mod inst_of_fancy_module(
        .\AFancySignalName[3].Something (\BFancySignalName.Something ),
        .\AFancySignalName[3].SomethingElse (\BFancySignalName.SomethingElse )
    );
endmodule
