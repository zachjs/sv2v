module Language.Verilog.Parser.Tokens
  ( Token     (..)
  , TokenName (..)
  , Position  (..)
  , tokenString
  ) where

import Text.Printf

tokenString :: Token -> String
tokenString (Token _ s _) = s

data Position = Position String Int Int deriving Eq

instance Show Position where
  show (Position f l c) = printf "%s:%d:%d" f l c

data Token = Token TokenName String Position deriving (Show, Eq)

data TokenName
  = KW_alias 
  | KW_always 
  | KW_always_comb 
  | KW_always_ff 
  | KW_always_latch 
  | KW_and 
  | KW_assert 
  | KW_assign 
  | KW_assume 
  | KW_automatic 
  | KW_before 
  | KW_begin 
  | KW_bind 
  | KW_bins 
  | KW_binsof 
  | KW_bit 
  | KW_break 
  | KW_buf 
  | KW_bufif0 
  | KW_bufif1 
  | KW_byte 
  | KW_case 
  | KW_casex 
  | KW_casez 
  | KW_cell 
  | KW_chandle 
  | KW_class 
  | KW_clocking 
  | KW_cmos 
  | KW_config 
  | KW_const 
  | KW_constraint 
  | KW_context 
  | KW_continue 
  | KW_cover 
  | KW_covergroup 
  | KW_coverpoint 
  | KW_cross 
  | KW_deassign 
  | KW_default 
  | KW_defparam 
  | KW_design 
  | KW_disable 
  | KW_dist 
  | KW_do 
  | KW_edge 
  | KW_else 
  | KW_end 
  | KW_endcase 
  | KW_endclass 
  | KW_endclocking 
  | KW_endconfig 
  | KW_endfunction 
  | KW_endgenerate 
  | KW_endgroup 
  | KW_endinterface 
  | KW_endmodule 
  | KW_endpackage 
  | KW_endprimitive 
  | KW_endprogram 
  | KW_endproperty 
  | KW_endspecify 
  | KW_endsequence 
  | KW_endtable 
  | KW_endtask 
  | KW_enum 
  | KW_event 
  | KW_expect 
  | KW_export 
  | KW_extends 
  | KW_extern 
  | KW_final 
  | KW_first_match 
  | KW_for 
  | KW_force 
  | KW_foreach 
  | KW_forever 
  | KW_fork 
  | KW_forkjoin 
  | KW_function 
  | KW_function_prototype 
  | KW_generate 
  | KW_genvar 
  | KW_highz0 
  | KW_highz1 
  | KW_if 
  | KW_iff 
  | KW_ifnone 
  | KW_ignore_bins 
  | KW_illegal_bins 
  | KW_import 
  | KW_incdir 
  | KW_include 
  | KW_initial 
  | KW_inout 
  | KW_input 
  | KW_inside 
  | KW_instance 
  | KW_int 
  | KW_integer 
  | KW_interface 
  | KW_intersect 
  | KW_join 
  | KW_join_any 
  | KW_join_none 
  | KW_large 
  | KW_liblist 
  | KW_library 
  | KW_local 
  | KW_localparam 
  | KW_logic 
  | KW_longint 
  | KW_macromodule 
  | KW_matches 
  | KW_medium 
  | KW_modport 
  | KW_module 
  | KW_nand 
  | KW_negedge 
  | KW_new 
  | KW_nmos 
  | KW_nor 
  | KW_noshowcancelled 
  | KW_not 
  | KW_notif0 
  | KW_notif1 
  | KW_null 
  | KW_option 
  | KW_or 
  | KW_output 
  | KW_package 
  | KW_packed 
  | KW_parameter 
  | KW_pathpulse_dollar 
  | KW_pmos 
  | KW_posedge 
  | KW_primitive 
  | KW_priority 
  | KW_program 
  | KW_property 
  | KW_protected 
  | KW_pull0 
  | KW_pull1 
  | KW_pulldown 
  | KW_pullup 
  | KW_pulsestyle_onevent 
  | KW_pulsestyle_ondetect 
  | KW_pure 
  | KW_rand 
  | KW_randc 
  | KW_randcase 
  | KW_randsequence 
  | KW_rcmos 
  | KW_real 
  | KW_realtime 
  | KW_ref 
  | KW_reg 
  | KW_release 
  | KW_repeat 
  | KW_return 
  | KW_rnmos 
  | KW_rpmos 
  | KW_rtran 
  | KW_rtranif0 
  | KW_rtranif1 
  | KW_scalared 
  | KW_sequence 
  | KW_shortint 
  | KW_shortreal 
  | KW_showcancelled 
  | KW_signed 
  | KW_small 
  | KW_solve 
  | KW_specify 
  | KW_specparam 
  | KW_static 
  | KW_strength0 
  | KW_strength1 
  | KW_string 
  | KW_strong0 
  | KW_strong1 
  | KW_struct 
  | KW_super 
  | KW_supply0 
  | KW_supply1 
  | KW_table 
  | KW_tagged 
  | KW_task 
  | KW_this 
  | KW_throughout 
  | KW_time 
  | KW_timeprecision 
  | KW_timeunit 
  | KW_tran 
  | KW_tranif0 
  | KW_tranif1 
  | KW_tri 
  | KW_tri0 
  | KW_tri1 
  | KW_triand 
  | KW_trior 
  | KW_trireg 
  | KW_type 
  | KW_typedef 
  | KW_type_option 
  | KW_union 
  | KW_unique 
  | KW_unsigned 
  | KW_use 
  | KW_var 
  | KW_vectored 
  | KW_virtual 
  | KW_void 
  | KW_wait 
  | KW_wait_order 
  | KW_wand 
  | KW_weak0 
  | KW_weak1 
  | KW_while 
  | KW_wildcard 
  | KW_wire 
  | KW_with 
  | KW_within 
  | KW_wor 
  | KW_xnor 
  | KW_xor 
  | Id_simple
  | Id_escaped
  | Id_system
  | Lit_number_unsigned
  | Lit_number
  | Lit_string
  | Sym_paren_l
  | Sym_paren_r
  | Sym_brack_l
  | Sym_brack_r
  | Sym_brace_l
  | Sym_brace_r
  | Sym_tildy
  | Sym_bang
  | Sym_at
  | Sym_pound
  | Sym_percent
  | Sym_hat
  | Sym_amp
  | Sym_bar
  | Sym_aster
  | Sym_dot
  | Sym_comma
  | Sym_colon
  | Sym_semi
  | Sym_eq
  | Sym_lt
  | Sym_gt
  | Sym_plus
  | Sym_dash
  | Sym_question
  | Sym_slash
  | Sym_dollar
  | Sym_s_quote
  | Sym_tildy_amp
  | Sym_tildy_bar
  | Sym_tildy_hat
  | Sym_hat_tildy
  | Sym_eq_eq
  | Sym_bang_eq
  | Sym_amp_amp
  | Sym_bar_bar
  | Sym_aster_aster
  | Sym_lt_eq
  | Sym_gt_eq
  | Sym_gt_gt
  | Sym_lt_lt
  | Sym_plus_plus
  | Sym_dash_dash
  | Sym_plus_eq
  | Sym_dash_eq
  | Sym_aster_eq
  | Sym_slash_eq
  | Sym_percent_eq
  | Sym_amp_eq
  | Sym_bar_eq
  | Sym_hat_eq
  | Sym_plus_colon
  | Sym_dash_colon
  | Sym_colon_colon
  | Sym_dot_aster
  | Sym_dash_gt
  | Sym_colon_eq
  | Sym_colon_slash
  | Sym_pound_pound
  | Sym_brack_l_aster
  | Sym_brack_l_eq
  | Sym_eq_gt
  | Sym_at_aster
  | Sym_paren_l_aster
  | Sym_aster_paren_r
  | Sym_aster_gt
  | Sym_eq_eq_eq
  | Sym_bang_eq_eq
  | Sym_eq_question_eq
  | Sym_bang_question_eq
  | Sym_gt_gt_gt
  | Sym_lt_lt_lt
  | Sym_lt_lt_eq
  | Sym_gt_gt_eq
  | Sym_bar_dash_gt
  | Sym_bar_eq_gt
  | Sym_brack_l_dash_gt
  | Sym_at_at_paren_l
  | Sym_paren_l_aster_paren_r
  | Sym_dash_gt_gt
  | Sym_amp_amp_amp
  | Sym_lt_lt_lt_eq
  | Sym_gt_gt_gt_eq
  | Unknown
  deriving (Show, Eq)

{-
keywordOrId :: String -> TokenName
keywordOrId s = findWithDefault Id_simple s keywords

keywords :: Map String TokenName
keywords = fromList
  [ ("alias", KW_alias)
  , ("always", KW_always)
  , ("always_comb", KW_always_comb)
  , ("always_ff", KW_always_ff)
  , ("always_latch", KW_always_latch)
  , ("and", KW_and)
  , ("assert", KW_assert)
  , ("assign", KW_assign)
  , ("assume", KW_assume)
  , ("automatic", KW_automatic)
  , ("before", KW_before)
  , ("begin", KW_begin)
  , ("bind", KW_bind)
  , ("bins", KW_bins)
  , ("binsof", KW_binsof)
  , ("bit", KW_bit)
  , ("break", KW_break)
  , ("buf", KW_buf)
  , ("bufif0", KW_bufif0)
  , ("bufif1", KW_bufif1)
  , ("byte", KW_byte)
  , ("case", KW_case)
  , ("casex", KW_casex)
  , ("casez", KW_casez)
  , ("cell", KW_cell)
  , ("chandle", KW_chandle)
  , ("class", KW_class)
  , ("clocking", KW_clocking)
  , ("cmos", KW_cmos)
  , ("config", KW_config)
  , ("const", KW_const)
  , ("constraint", KW_constraint)
  , ("context", KW_context)
  , ("continue", KW_continue)
  , ("cover", KW_cover)
  , ("covergroup", KW_covergroup)
  , ("coverpoint", KW_coverpoint)
  , ("cross", KW_cross)
  , ("deassign", KW_deassign)
  , ("default", KW_default)
  , ("defparam", KW_defparam)
  , ("design", KW_design)
  , ("disable", KW_disable)
  , ("dist", KW_dist)
  , ("do", KW_do)
  , ("edge", KW_edge)
  , ("else", KW_else)
  , ("end", KW_end)
  , ("endcase", KW_endcase)
  , ("endclass", KW_endclass)
  , ("endclocking", KW_endclocking)
  , ("endconfig", KW_endconfig)
  , ("endfunction", KW_endfunction)
  , ("endgenerate", KW_endgenerate)
  , ("endgroup", KW_endgroup)
  , ("endinterface", KW_endinterface)
  , ("endmodule", KW_endmodule)
  , ("endpackage", KW_endpackage)
  , ("endprimitive", KW_endprimitive)
  , ("endprogram", KW_endprogram)
  , ("endproperty", KW_endproperty)
  , ("endspecify", KW_endspecify)
  , ("endsequence", KW_endsequence)
  , ("endtable", KW_endtable)
  , ("endtask", KW_endtask)
  , ("enum", KW_enum)
  , ("event", KW_event)
  , ("expect", KW_expect)
  , ("export", KW_export)
  , ("extends", KW_extends)
  , ("extern", KW_extern)
  , ("final", KW_final)
  , ("first_match", KW_first_match)
  , ("for", KW_for)
  , ("force", KW_force)
  , ("foreach", KW_foreach)
  , ("forever", KW_forever)
  , ("fork", KW_fork)
  , ("forkjoin", KW_forkjoin)
  , ("function", KW_function)
  , ("function_prototype", KW_function_prototype)
  , ("generate", KW_generate)
  , ("genvar", KW_genvar)
  , ("highz0", KW_highz0)
  , ("highz1", KW_highz1)
  , ("if", KW_if)
  , ("iff", KW_iff)
  , ("ifnone", KW_ifnone)
  , ("ignore_bins", KW_ignore_bins)
  , ("illegal_bins", KW_illegal_bins)
  , ("import", KW_import)
  , ("incdir", KW_incdir)
  , ("include", KW_include)
  , ("initial", KW_initial)
  , ("inout", KW_inout)
  , ("input", KW_input)
  , ("inside", KW_inside)
  , ("instance", KW_instance)
  , ("int", KW_int)
  , ("integer", KW_integer)
  , ("interface", KW_interface)
  , ("intersect", KW_intersect)
  , ("join", KW_join)
  , ("join_any", KW_join_any)
  , ("join_none", KW_join_none)
  , ("large", KW_large)
  , ("liblist", KW_liblist)
  , ("library", KW_library)
  , ("local", KW_local)
  , ("localparam", KW_localparam)
  , ("logic", KW_logic)
  , ("longint", KW_longint)
  , ("macromodule", KW_macromodule)
  , ("matches", KW_matches)
  , ("medium", KW_medium)
  , ("modport", KW_modport)
  , ("module", KW_module)
  , ("nand", KW_nand)
  , ("negedge", KW_negedge)
  , ("new", KW_new)
  , ("nmos", KW_nmos)
  , ("nor", KW_nor)
  , ("noshowcancelled", KW_noshowcancelled)
  , ("not", KW_not)
  , ("notif0", KW_notif0)
  , ("notif1", KW_notif1)
  , ("null", KW_null)
  , ("option", KW_option)
  , ("or", KW_or)
  , ("output", KW_output)
  , ("package", KW_package)
  , ("packed", KW_packed)
  , ("parameter", KW_parameter)
  , ("pathpulse_dollar", KW_pathpulse_dollar)
  , ("pmos", KW_pmos)
  , ("posedge", KW_posedge)
  , ("primitive", KW_primitive)
  , ("priority", KW_priority)
  , ("program", KW_program)
  , ("property", KW_property)
  , ("protected", KW_protected)
  , ("pull0", KW_pull0)
  , ("pull1", KW_pull1)
  , ("pulldown", KW_pulldown)
  , ("pullup", KW_pullup)
  , ("pulsestyle_onevent", KW_pulsestyle_onevent)
  , ("pulsestyle_ondetect", KW_pulsestyle_ondetect)
  , ("pure", KW_pure)
  , ("rand", KW_rand)
  , ("randc", KW_randc)
  , ("randcase", KW_randcase)
  , ("randsequence", KW_randsequence)
  , ("rcmos", KW_rcmos)
  , ("real", KW_real)
  , ("realtime", KW_realtime)
  , ("ref", KW_ref)
  , ("reg", KW_reg)
  , ("release", KW_release)
  , ("repeat", KW_repeat)
  , ("return", KW_return)
  , ("rnmos", KW_rnmos)
  , ("rpmos", KW_rpmos)
  , ("rtran", KW_rtran)
  , ("rtranif0", KW_rtranif0)
  , ("rtranif1", KW_rtranif1)
  , ("scalared", KW_scalared)
  , ("sequence", KW_sequence)
  , ("shortint", KW_shortint)
  , ("shortreal", KW_shortreal)
  , ("showcancelled", KW_showcancelled)
  , ("signed", KW_signed)
  , ("small", KW_small)
  , ("solve", KW_solve)
  , ("specify", KW_specify)
  , ("specparam", KW_specparam)
  , ("static", KW_static)
  , ("strength0", KW_strength0)
  , ("strength1", KW_strength1)
  , ("string", KW_string)
  , ("strong0", KW_strong0)
  , ("strong1", KW_strong1)
  , ("struct", KW_struct)
  , ("super", KW_super)
  , ("supply0", KW_supply0)
  , ("supply1", KW_supply1)
  , ("table", KW_table)
  , ("tagged", KW_tagged)
  , ("task", KW_task)
  , ("this", KW_this)
  , ("throughout", KW_throughout)
  , ("time", KW_time)
  , ("timeprecision", KW_timeprecision)
  , ("timeunit", KW_timeunit)
  , ("tran", KW_tran)
  , ("tranif0", KW_tranif0)
  , ("tranif1", KW_tranif1)
  , ("tri", KW_tri)
  , ("tri0", KW_tri0)
  , ("tri1", KW_tri1)
  , ("triand", KW_triand)
  , ("trior", KW_trior)
  , ("trireg", KW_trireg)
  , ("type", KW_type)
  , ("typedef", KW_typedef)
  , ("type_option", KW_type_option)
  , ("union", KW_union)
  , ("unique", KW_unique)
  , ("unsigned", KW_unsigned)
  , ("use", KW_use)
  , ("var", KW_var)
  , ("vectored", KW_vectored)
  , ("virtual", KW_virtual)
  , ("void", KW_void)
  , ("wait", KW_wait)
  , ("wait_order", KW_wait_order)
  , ("wand", KW_wand)
  , ("weak0", KW_weak0)
  , ("weak1", KW_weak1)
  , ("while", KW_while)
  , ("wildcard", KW_wildcard)
  , ("wire", KW_wire)
  , ("with", KW_with)
  , ("within", KW_within)
  , ("wor", KW_wor)
  , ("xnor", KW_xnor)
  , ("xor", KW_xor)
  ]

  -- \$fullskew
  -- \$hold
  -- \$nochange
  -- \$period
  -- \$randomize
  -- \$recovery
  -- \$recrem
  -- \$removal
  -- \$root
  -- \$setup
  -- \$setuphold
  -- \$skew
  -- \$timeskew
  -- \$unit
  -- \$width
-}
