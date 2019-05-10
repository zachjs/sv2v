typedef struct packed { reg w; bit x; logic y; } StructA;
typedef struct packed { reg w; logic y; bit x; } StructB;
typedef struct packed { bit x; reg w; logic y; } StructC;
typedef struct packed { logic y; reg w; bit x; } StructD;
typedef struct packed { bit x; logic y; reg w; } StructE;
typedef struct packed { logic y; bit x; reg w; } StructF;

module top;

    integer i, j, k;
    StructA a;
    StructB b;
    StructC c;
    StructD d;
    StructE e;
    StructF f;
    initial begin
        for (i = 0; i < 2; i++) begin
            for (j = 0; j < 2; j++) begin
                for (k = 0; k < 2; k++) begin
                    a = '{ w:i, x:j, y:k };
                    b = '{ w:i, x:j, y:k };
                    c = '{ w:i, x:j, y:k };
                    d = '{ w:i, x:j, y:k };
                    e = '{ w:i, x:j, y:k };
                    f = '{ w:i, x:j, y:k };
                    $display("A: %1d%1d%1d -> ", i,j,k, a,b,c,d,e,f);
                end
            end
        end
    end

    initial begin
        integer i, j, k;
        StructB a;
        StructC b;
        StructD c;
        StructE d;
        StructF e;
        StructA f;
        for (i = 0; i < 2; i++) begin
            for (j = 0; j < 2; j++) begin
                for (k = 0; k < 2; k++) begin
                    a = '{ w:i, x:j, y:k };
                    b = '{ w:i, x:j, y:k };
                    c = '{ w:i, x:j, y:k };
                    d = '{ w:i, x:j, y:k };
                    e = '{ w:i, x:j, y:k };
                    f = '{ w:i, x:j, y:k };
                    $display("B: %1d%1d%1d -> ", i,j,k, a,b,c,d,e,f);
                end
            end
        end

        begin
            integer i, j, k;
            StructC a;
            StructD b;
            StructE c;
            StructF d;
            StructA e;
            StructB f;
            for (i = 0; i < 2; i++) begin
                for (j = 0; j < 2; j++) begin
                    for (k = 0; k < 2; k++) begin
                        a = '{ w:i, x:j, y:k };
                        b = '{ w:i, x:j, y:k };
                        c = '{ w:i, x:j, y:k };
                        d = '{ w:i, x:j, y:k };
                        e = '{ w:i, x:j, y:k };
                        f = '{ w:i, x:j, y:k };
                        $display("C: %1d%1d%1d -> ", i,j,k, a,b,c,d,e,f);
                    end
                end
            end
        end
    end

    task foo;
        integer i, j, k;
        StructD a;
        StructE b;
        StructF c;
        StructA d;
        StructB e;
        StructC f;
        for (i = 0; i < 2; i++) begin
            for (j = 0; j < 2; j++) begin
                for (k = 0; k < 2; k++) begin
                    a = '{ w:i, x:j, y:k };
                    b = '{ w:i, x:j, y:k };
                    c = '{ w:i, x:j, y:k };
                    d = '{ w:i, x:j, y:k };
                    e = '{ w:i, x:j, y:k };
                    f = '{ w:i, x:j, y:k };
                    $display("D: %1d%1d%1d -> ", i,j,k, a,b,c,d,e,f);
                end
            end
        end
    endtask
    initial foo();

    task bar;
        input integer i, j, k;
        input StructE a;
        input StructF b;
        input StructA c;
        input StructB d;
        input StructC e;
        input StructD f;
        $display("E: %1d%1d%1d -> ", i,j,k, a,b,c,d,e,f);
    endtask
    initial begin
        integer i, j, k;
        for (i = 0; i < 2; i++) begin
            for (j = 0; j < 2; j++) begin
                for (k = 0; k < 2; k++) begin
                    bar(i,j,k
                        , '{ w:i, x:j, y:k }
                        , '{ w:i, x:j, y:k }
                        , '{ w:i, x:j, y:k }
                        , '{ w:i, x:j, y:k }
                        , '{ w:i, x:j, y:k }
                        , '{ w:i, x:j, y:k }
                    );
                end
            end
        end
    end

    function baz;
        input integer i, j, k;
        input StructF a;
        input StructA b;
        input StructB c;
        input StructC d;
        input StructD e;
        input StructE f;
        $display("F: %1d%1d%1d -> ", i,j,k, a,b,c,d,e,f);
        baz = 0;
    endfunction
    initial begin
        integer i, j, k;
        integer unused;
        for (i = 0; i < 2; i++) begin
            for (j = 0; j < 2; j++) begin
                for (k = 0; k < 2; k++) begin
                    unused = baz(i,j,k
                        , '{ w:i, x:j, y:k }
                        , '{ w:i, x:j, y:k }
                        , '{ w:i, x:j, y:k }
                        , '{ w:i, x:j, y:k }
                        , '{ w:i, x:j, y:k }
                        , '{ w:i, x:j, y:k }
                    );
                end
            end
        end
    end
endmodule
