package pkg;
    `define STRUCT \
        struct packed { \
            integer unsigned a, b, c, d, e, f, g, h, i, j, k, l, m, \
                n, o, p, q, r, s, t, u, v, w, x, y, z; \
        }
    typedef `STRUCT T;

    `define step(b, o, f) f: o.f == "inv" ? b.f : o.f
    `define extend(_b, _o) '{ \
        `step(_b, _o, a), \
        `step(_b, _o, b), \
        `step(_b, _o, c), \
        `step(_b, _o, d), \
        `step(_b, _o, e), \
        `step(_b, _o, f), \
        `step(_b, _o, g), \
        `step(_b, _o, h), \
        `step(_b, _o, i), \
        `step(_b, _o, j), \
        `step(_b, _o, k), \
        `step(_b, _o, l), \
        `step(_b, _o, m), \
        `step(_b, _o, n), \
        `step(_b, _o, o), \
        `step(_b, _o, p), \
        `step(_b, _o, q), \
        `step(_b, _o, r), \
        `step(_b, _o, s), \
        `step(_b, _o, t), \
        `step(_b, _o, u), \
        `step(_b, _o, v), \
        `step(_b, _o, w), \
        `step(_b, _o, x), \
        `step(_b, _o, y), \
        `step(_b, _o, z) \
    }

    localparam X = 1'd0;
    localparam Y = 1'd1;

    localparam T a1_cfg = '{a: X, b: X, c: X, d: X, e: X, f: X, default: Y};
    localparam `STRUCT a2_cfg = a1_cfg;

    `define expand(let_a, let_b) \
        localparam T let_a``1_ext = '{let_a: Y, default: "inv"}; \
        localparam T let_b``1_cfg = `extend(let_a``1_cfg, let_a``1_ext); \
        localparam `STRUCT let_a``2_ext = '{let_a: Y, default: "inv"}; \
        localparam `STRUCT let_b``2_cfg = `extend(let_a``2_cfg, let_a``2_ext);

    `expand(a, b)
    `expand(b, c)
    `expand(c, d)
    `expand(d, e)
    `expand(e, f)
    `expand(f, g)
    `expand(g, h)
    `expand(h, i)
    `expand(i, j)
    `expand(j, k)
    `expand(k, l)
    `expand(l, m)
    `expand(m, n)
    `expand(n, o)
    `expand(o, p)
    `expand(p, q)
    `expand(q, r)
    `expand(r, s)
    `expand(s, t)
    `expand(t, u)
    `expand(u, v)
    `expand(v, w)
    `expand(w, x)
    `expand(x, y)
    `expand(y, z)

    localparam P = z1_cfg.z;
    localparam Q = 3 * z2_cfg.z;
    localparam `STRUCT R = '0;
endpackage

module top;
    initial $display(pkg::P, pkg::Q, pkg::R);
endmodule
