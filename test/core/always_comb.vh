reg never;
`define TEST(sense, num) \
    reg x``num, y``num, z``num; \
    function automatic t``num; \
        input inp; \
        t``num = x``num; \
    endfunction \
    always``sense begin \
        y``num = 0; \
        z``num = t``num(0); \
        if (never) ; \
    end
