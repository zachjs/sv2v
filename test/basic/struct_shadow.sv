typedef struct packed { logic w, x, y; } StructA;
typedef struct packed { logic w, y, x; } StructB;
typedef struct packed { logic x, w, y; } StructC;
typedef struct packed { logic y, w, x; } StructD;
typedef struct packed { logic x, y, w; } StructE;
typedef struct packed { logic y, x, w; } StructF;

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

endmodule
