{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - To implement the `begin_keywords` directive, this module defines which IEEE
 - 1800-2017 keywords are not keywords in preceding specifications.
 -}
module Language.SystemVerilog.Parser.Keywords
    ( specMap
    ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Language.SystemVerilog.Parser.Tokens

newKeywords :: [(String, [TokenName])]
newKeywords = [

    ("1364-1995", [KW_always, KW_and, KW_assign, KW_begin, KW_buf, KW_bufif0,
    KW_bufif1, KW_case, KW_casex, KW_casez, KW_cmos, KW_deassign, KW_default,
    KW_defparam, KW_disable, KW_edge, KW_else, KW_end, KW_endcase,
    KW_endfunction, KW_endmodule, KW_endprimitive, KW_endspecify, KW_endtable,
    KW_endtask, KW_event, KW_for, KW_force, KW_forever, KW_fork, KW_function,
    KW_highz0, KW_highz1, KW_if, KW_ifnone, KW_initial, KW_inout, KW_input,
    KW_integer, KW_join, KW_large, KW_macromodule, KW_medium, KW_module,
    KW_nand, KW_negedge, KW_nmos, KW_nor, KW_not, KW_notif0, KW_notif1, KW_or,
    KW_output, KW_parameter, KW_pmos, KW_posedge, KW_primitive, KW_pull0,
    KW_pull1, KW_pulldown, KW_pullup, KW_rcmos, KW_real, KW_realtime, KW_reg,
    KW_release, KW_repeat, KW_rnmos, KW_rpmos, KW_rtran, KW_rtranif0,
    KW_rtranif1, KW_scalared, KW_small, KW_specify, KW_specparam, KW_strong0,
    KW_strong1, KW_supply0, KW_supply1, KW_table, KW_task, KW_time, KW_tran,
    KW_tranif0, KW_tranif1, KW_tri, KW_tri0, KW_tri1, KW_triand, KW_trior,
    KW_trireg, KW_vectored, KW_wait, KW_wand, KW_weak0, KW_weak1, KW_while,
    KW_wire, KW_wor, KW_xnor, KW_xor]),

    ("1364-2001-noconfig", [KW_cell, KW_config, KW_design, KW_endconfig,
    KW_incdir, KW_include, KW_instance, KW_liblist, KW_library, KW_use]),

    ("1364-2001", [KW_automatic, KW_endgenerate, KW_generate, KW_genvar,
    KW_localparam, KW_noshowcancelled, KW_pulsestyle_ondetect,
    KW_pulsestyle_onevent, KW_showcancelled, KW_signed, KW_unsigned]),

    ("1364-2005", [KW_uwire]),

    ("1800-2005", [KW_alias, KW_always_comb, KW_always_ff, KW_always_latch,
    KW_assert, KW_assume, KW_before, KW_bind, KW_bins, KW_binsof, KW_bit,
    KW_break, KW_byte, KW_chandle, KW_class, KW_clocking, KW_const,
    KW_constraint, KW_context, KW_continue, KW_cover, KW_covergroup,
    KW_coverpoint, KW_cross, KW_dist, KW_do, KW_endclass, KW_endclocking,
    KW_endgroup, KW_endinterface, KW_endpackage, KW_endprogram, KW_endproperty,
    KW_endsequence, KW_enum, KW_expect, KW_export, KW_extends, KW_extern,
    KW_final, KW_first_match, KW_foreach, KW_forkjoin, KW_iff, KW_ignore_bins,
    KW_illegal_bins, KW_import, KW_inside, KW_int, KW_interface, KW_intersect,
    KW_join_any, KW_join_none, KW_local, KW_logic, KW_longint, KW_matches,
    KW_modport, KW_new, KW_null, KW_package, KW_packed, KW_priority, KW_program,
    KW_property, KW_protected, KW_pure, KW_rand, KW_randc, KW_randcase,
    KW_randsequence, KW_ref, KW_return, KW_sequence, KW_shortint, KW_shortreal,
    KW_solve, KW_static, KW_string, KW_struct, KW_super, KW_tagged, KW_this,
    KW_throughout, KW_timeprecision, KW_timeunit, KW_type, KW_typedef, KW_union,
    KW_unique, KW_var, KW_virtual, KW_void, KW_wait_order, KW_wildcard, KW_with,
    KW_within]),

    ("1800-2009", [KW_accept_on, KW_checker, KW_endchecker, KW_eventually,
    KW_global, KW_implies, KW_let, KW_nexttime, KW_reject_on, KW_restrict,
    KW_s_always, KW_s_eventually, KW_s_nexttime, KW_s_until, KW_s_until_with,
    KW_strong, KW_sync_accept_on, KW_sync_reject_on, KW_unique0, KW_until,
    KW_until_with, KW_untyped, KW_weak]),

    ("1800-2012", [KW_implements, KW_nettype, KW_interconnect, KW_soft]),

    ("1800-2017", [])

    ]

specMap :: Map.Map String (Set.Set TokenName)
specMap =
    Map.fromList $ zip keys vals
    where
        keys = map fst newKeywords
        sets = map (Set.fromList . snd) newKeywords
        allKeywords = Set.unions sets
        vals =
            map (Set.difference allKeywords) $
            scanl1 Set.union sets
