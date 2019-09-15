{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 - Original Parser Author: Tom Hawkins <tomahawkins@gmail.com>
 -
 - This file has been *heavily* modified and extended from the original version
 - in tomahawkins/verilog. I have added support for numerous SystemVerilog
 - constructs, which has necessitated rewriting nearly all of this.
 -
 - This file is the only remaining one that still uses 2-space indentation. I've
 - decided to leave it this way because I think it is too important to preserve
 - the ability to easily blame/diff this file.
 -}
{
module Language.SystemVerilog.Parser.Parse (parse) where

import Language.SystemVerilog.AST
import Language.SystemVerilog.Parser.ParseDecl
import Language.SystemVerilog.Parser.Tokens
}

%name parse
%tokentype { Token }
%error { parseError }

%expect 0

%token

"$bits"                { Token KW_dollar_bits                _ _ }
"$dimensions"          { Token KW_dollar_dimensions          _ _ }
"$unpacked_dimensions" { Token KW_dollar_unpacked_dimensions _ _ }
"$left"                { Token KW_dollar_left                _ _ }
"$right"               { Token KW_dollar_right               _ _ }
"$low"                 { Token KW_dollar_low                 _ _ }
"$high"                { Token KW_dollar_high                _ _ }
"$increment"           { Token KW_dollar_increment           _ _ }
"$size"                { Token KW_dollar_size                _ _ }

"accept_on"        { Token KW_accept_on    _ _ }
"alias"            { Token KW_alias        _ _ }
"always"           { Token KW_always       _ _ }
"always_comb"      { Token KW_always_comb  _ _ }
"always_ff"        { Token KW_always_ff    _ _ }
"always_latch"     { Token KW_always_latch _ _ }
"and"              { Token KW_and          _ _ }
"assert"           { Token KW_assert       _ _ }
"assign"           { Token KW_assign       _ _ }
"assume"           { Token KW_assume       _ _ }
"automatic"        { Token KW_automatic    _ _ }
"before"           { Token KW_before       _ _ }
"begin"            { Token KW_begin        _ _ }
"bind"             { Token KW_bind         _ _ }
"bins"             { Token KW_bins         _ _ }
"binsof"           { Token KW_binsof       _ _ }
"bit"              { Token KW_bit          _ _ }
"break"            { Token KW_break        _ _ }
"buf"              { Token KW_buf          _ _ }
"bufif0"           { Token KW_bufif0       _ _ }
"bufif1"           { Token KW_bufif1       _ _ }
"byte"             { Token KW_byte         _ _ }
"case"             { Token KW_case         _ _ }
"casex"            { Token KW_casex        _ _ }
"casez"            { Token KW_casez        _ _ }
"cell"             { Token KW_cell         _ _ }
"chandle"          { Token KW_chandle      _ _ }
"checker"          { Token KW_checker      _ _ }
"class"            { Token KW_class        _ _ }
"clocking"         { Token KW_clocking     _ _ }
"cmos"             { Token KW_cmos         _ _ }
"config"           { Token KW_config       _ _ }
"const"            { Token KW_const        _ _ }
"constraint"       { Token KW_constraint   _ _ }
"context"          { Token KW_context      _ _ }
"continue"         { Token KW_continue     _ _ }
"cover"            { Token KW_cover        _ _ }
"covergroup"       { Token KW_covergroup   _ _ }
"coverpoint"       { Token KW_coverpoint   _ _ }
"cross"            { Token KW_cross        _ _ }
"deassign"         { Token KW_deassign     _ _ }
"default"          { Token KW_default      _ _ }
"defparam"         { Token KW_defparam     _ _ }
"design"           { Token KW_design       _ _ }
"disable"          { Token KW_disable      _ _ }
"dist"             { Token KW_dist         _ _ }
"do"               { Token KW_do           _ _ }
"edge"             { Token KW_edge         _ _ }
"else"             { Token KW_else         _ _ }
"end"              { Token KW_end          _ _ }
"endcase"          { Token KW_endcase      _ _ }
"endchecker"       { Token KW_endchecker   _ _ }
"endclass"         { Token KW_endclass     _ _ }
"endclocking"      { Token KW_endclocking  _ _ }
"endconfig"        { Token KW_endconfig    _ _ }
"endfunction"      { Token KW_endfunction  _ _ }
"endgenerate"      { Token KW_endgenerate  _ _ }
"endgroup"         { Token KW_endgroup     _ _ }
"endinterface"     { Token KW_endinterface _ _ }
"endmodule"        { Token KW_endmodule    _ _ }
"endpackage"       { Token KW_endpackage   _ _ }
"endprimitive"     { Token KW_endprimitive _ _ }
"endprogram"       { Token KW_endprogram   _ _ }
"endproperty"      { Token KW_endproperty  _ _ }
"endspecify"       { Token KW_endspecify   _ _ }
"endsequence"      { Token KW_endsequence  _ _ }
"endtable"         { Token KW_endtable     _ _ }
"endtask"          { Token KW_endtask      _ _ }
"enum"             { Token KW_enum         _ _ }
"event"            { Token KW_event        _ _ }
"eventually"       { Token KW_eventually   _ _ }
"expect"           { Token KW_expect       _ _ }
"export"           { Token KW_export       _ _ }
"extends"          { Token KW_extends      _ _ }
"extern"           { Token KW_extern       _ _ }
"final"            { Token KW_final        _ _ }
"first_match"      { Token KW_first_match  _ _ }
"for"              { Token KW_for          _ _ }
"force"            { Token KW_force        _ _ }
"foreach"          { Token KW_foreach      _ _ }
"forever"          { Token KW_forever      _ _ }
"fork"             { Token KW_fork         _ _ }
"forkjoin"         { Token KW_forkjoin     _ _ }
"function"         { Token KW_function     _ _ }
"generate"         { Token KW_generate     _ _ }
"genvar"           { Token KW_genvar       _ _ }
"global"           { Token KW_global       _ _ }
"highz0"           { Token KW_highz0       _ _ }
"highz1"           { Token KW_highz1       _ _ }
"if"               { Token KW_if           _ _ }
"iff"              { Token KW_iff          _ _ }
"ifnone"           { Token KW_ifnone       _ _ }
"ignore_bins"      { Token KW_ignore_bins  _ _ }
"illegal_bins"     { Token KW_illegal_bins _ _ }
"implements"       { Token KW_implements   _ _ }
"implies"          { Token KW_implies      _ _ }
"import"           { Token KW_import       _ _ }
"incdir"           { Token KW_incdir       _ _ }
"include"          { Token KW_include      _ _ }
"initial"          { Token KW_initial      _ _ }
"inout"            { Token KW_inout        _ _ }
"input"            { Token KW_input        _ _ }
"inside"           { Token KW_inside       _ _ }
"instance"         { Token KW_instance     _ _ }
"int"              { Token KW_int          _ _ }
"integer"          { Token KW_integer      _ _ }
"interconnect"     { Token KW_interconnect _ _ }
"interface"        { Token KW_interface    _ _ }
"intersect"        { Token KW_intersect    _ _ }
"join"             { Token KW_join         _ _ }
"join_any"         { Token KW_join_any     _ _ }
"join_none"        { Token KW_join_none    _ _ }
"large"            { Token KW_large        _ _ }
"let"              { Token KW_let          _ _ }
"liblist"          { Token KW_liblist      _ _ }
"library"          { Token KW_library      _ _ }
"local"            { Token KW_local        _ _ }
"localparam"       { Token KW_localparam   _ _ }
"logic"            { Token KW_logic        _ _ }
"longint"          { Token KW_longint      _ _ }
"macromodule"      { Token KW_macromodule  _ _ }
"matches"          { Token KW_matches      _ _ }
"medium"           { Token KW_medium       _ _ }
"modport"          { Token KW_modport      _ _ }
"module"           { Token KW_module       _ _ }
"nand"             { Token KW_nand         _ _ }
"negedge"          { Token KW_negedge      _ _ }
"nettype"          { Token KW_nettype      _ _ }
"new"              { Token KW_new          _ _ }
"nexttime"         { Token KW_nexttime     _ _ }
"nmos"             { Token KW_nmos         _ _ }
"nor"              { Token KW_nor          _ _ }
"noshowcancelled"  { Token KW_noshowcancelled _ _ }
"not"              { Token KW_not          _ _ }
"notif0"           { Token KW_notif0       _ _ }
"notif1"           { Token KW_notif1       _ _ }
"null"             { Token KW_null         _ _ }
"or"               { Token KW_or           _ _ }
"output"           { Token KW_output       _ _ }
"package"          { Token KW_package      _ _ }
"packed"           { Token KW_packed       _ _ }
"parameter"        { Token KW_parameter    _ _ }
"pmos"             { Token KW_pmos         _ _ }
"posedge"          { Token KW_posedge      _ _ }
"primitive"        { Token KW_primitive    _ _ }
"priority"         { Token KW_priority     _ _ }
"program"          { Token KW_program      _ _ }
"property"         { Token KW_property     _ _ }
"protected"        { Token KW_protected    _ _ }
"pull0"            { Token KW_pull0        _ _ }
"pull1"            { Token KW_pull1        _ _ }
"pulldown"         { Token KW_pulldown     _ _ }
"pullup"           { Token KW_pullup       _ _ }
"pulsestyle_ondetect" { Token KW_pulsestyle_ondetect _ _ }
"pulsestyle_onevent"  { Token KW_pulsestyle_onevent _ _ }
"pure"             { Token KW_pure         _ _ }
"rand"             { Token KW_rand         _ _ }
"randc"            { Token KW_randc        _ _ }
"randcase"         { Token KW_randcase     _ _ }
"randsequence"     { Token KW_randsequence _ _ }
"rcmos"            { Token KW_rcmos        _ _ }
"real"             { Token KW_real         _ _ }
"realtime"         { Token KW_realtime     _ _ }
"ref"              { Token KW_ref          _ _ }
"reg"              { Token KW_reg          _ _ }
"reject_on"        { Token KW_reject_on    _ _ }
"release"          { Token KW_release      _ _ }
"repeat"           { Token KW_repeat       _ _ }
"restrict"         { Token KW_restrict     _ _ }
"return"           { Token KW_return       _ _ }
"rnmos"            { Token KW_rnmos        _ _ }
"rpmos"            { Token KW_rpmos        _ _ }
"rtran"            { Token KW_rtran        _ _ }
"rtranif0"         { Token KW_rtranif0     _ _ }
"rtranif1"         { Token KW_rtranif1     _ _ }
"s_always"         { Token KW_s_always     _ _ }
"s_eventually"     { Token KW_s_eventually _ _ }
"s_nexttime"       { Token KW_s_nexttime   _ _ }
"s_until"          { Token KW_s_until      _ _ }
"s_until_with"     { Token KW_s_until_with _ _ }
"scalared"         { Token KW_scalared     _ _ }
"sequence"         { Token KW_sequence     _ _ }
"shortint"         { Token KW_shortint     _ _ }
"shortreal"        { Token KW_shortreal    _ _ }
"showcancelled"    { Token KW_showcancelled _ _ }
"signed"           { Token KW_signed       _ _ }
"small"            { Token KW_small        _ _ }
"soft"             { Token KW_soft         _ _ }
"solve"            { Token KW_solve        _ _ }
"specify"          { Token KW_specify      _ _ }
"specparam"        { Token KW_specparam    _ _ }
"static"           { Token KW_static       _ _ }
"string"           { Token KW_string       _ _ }
"strong"           { Token KW_strong       _ _ }
"strong0"          { Token KW_strong0      _ _ }
"strong1"          { Token KW_strong1      _ _ }
"struct"           { Token KW_struct       _ _ }
"super"            { Token KW_super        _ _ }
"supply0"          { Token KW_supply0      _ _ }
"supply1"          { Token KW_supply1      _ _ }
"sync_accept_on"   { Token KW_sync_accept_on _ _ }
"sync_reject_on"   { Token KW_sync_reject_on _ _ }
"table"            { Token KW_table        _ _ }
"tagged"           { Token KW_tagged       _ _ }
"task"             { Token KW_task         _ _ }
"this"             { Token KW_this         _ _ }
"throughout"       { Token KW_throughout   _ _ }
"time"             { Token KW_time         _ _ }
"timeprecision"    { Token KW_timeprecision _ _ }
"timeunit"         { Token KW_timeunit     _ _ }
"tran"             { Token KW_tran         _ _ }
"tranif0"          { Token KW_tranif0      _ _ }
"tranif1"          { Token KW_tranif1      _ _ }
"tri"              { Token KW_tri          _ _ }
"tri0"             { Token KW_tri0         _ _ }
"tri1"             { Token KW_tri1         _ _ }
"triand"           { Token KW_triand       _ _ }
"trior"            { Token KW_trior        _ _ }
"trireg"           { Token KW_trireg       _ _ }
"type"             { Token KW_type         _ _ }
"typedef"          { Token KW_typedef      _ _ }
"union"            { Token KW_union        _ _ }
"unique"           { Token KW_unique       _ _ }
"unique0"          { Token KW_unique0      _ _ }
"unsigned"         { Token KW_unsigned     _ _ }
"until"            { Token KW_until        _ _ }
"until_with"       { Token KW_until_with   _ _ }
"untyped"          { Token KW_untyped      _ _ }
"use"              { Token KW_use          _ _ }
"uwire"            { Token KW_uwire        _ _ }
"var"              { Token KW_var          _ _ }
"vectored"         { Token KW_vectored     _ _ }
"virtual"          { Token KW_virtual      _ _ }
"void"             { Token KW_void         _ _ }
"wait"             { Token KW_wait         _ _ }
"wait_order"       { Token KW_wait_order   _ _ }
"wand"             { Token KW_wand         _ _ }
"weak"             { Token KW_weak         _ _ }
"weak0"            { Token KW_weak0        _ _ }
"weak1"            { Token KW_weak1        _ _ }
"while"            { Token KW_while        _ _ }
"wildcard"         { Token KW_wildcard     _ _ }
"wire"             { Token KW_wire         _ _ }
"with"             { Token KW_with         _ _ }
"within"           { Token KW_within       _ _ }
"wor"              { Token KW_wor          _ _ }
"xnor"             { Token KW_xnor         _ _ }
"xor"              { Token KW_xor          _ _ }

simpleIdentifier   { Token Id_simple       _ _ }
escapedIdentifier  { Token Id_escaped      _ _ }
systemIdentifier   { Token Id_system       _ _ }
number             { Token Lit_number      _ _ }
string             { Token Lit_string      _ _ }
time               { Token Lit_time        _ _ }

"("                { Token Sym_paren_l _ _ }
")"                { Token Sym_paren_r _ _ }
"["                { Token Sym_brack_l _ _ }
"]"                { Token Sym_brack_r _ _ }
"{"                { Token Sym_brace_l _ _ }
"}"                { Token Sym_brace_r _ _ }
"~"                { Token Sym_tildy _ _ }
"!"                { Token Sym_bang _ _ }
"@"                { Token Sym_at _ _ }
"#"                { Token Sym_pound _ _ }
"%"                { Token Sym_percent _ _ }
"^"                { Token Sym_hat _ _ }
"&"                { Token Sym_amp _ _ }
"|"                { Token Sym_bar _ _ }
"*"                { Token Sym_aster _ _ }
"."                { Token Sym_dot _ _ }
","                { Token Sym_comma _ _ }
":"                { Token Sym_colon _ _ }
";"                { Token Sym_semi _ _ }
"="                { Token Sym_eq _ _ }
"<"                { Token Sym_lt _ _ }
">"                { Token Sym_gt _ _ }
"+"                { Token Sym_plus _ _ }
"-"                { Token Sym_dash _ _ }
"?"                { Token Sym_question _ _ }
"/"                { Token Sym_slash _ _ }
"$"                { Token Sym_dollar _ _ }
"'"                { Token Sym_s_quote _ _ }
"~&"               { Token Sym_tildy_amp _ _ }
"~|"               { Token Sym_tildy_bar _ _ }
"~^"               { Token Sym_tildy_hat _ _ }
"^~"               { Token Sym_hat_tildy _ _ }
"=="               { Token Sym_eq_eq _ _ }
"!="               { Token Sym_bang_eq _ _ }
"&&"               { Token Sym_amp_amp _ _ }
"||"               { Token Sym_bar_bar _ _ }
"**"               { Token Sym_aster_aster _ _ }
"<="               { Token Sym_lt_eq _ _ }
">="               { Token Sym_gt_eq _ _ }
">>"               { Token Sym_gt_gt _ _ }
"<<"               { Token Sym_lt_lt _ _ }
"++"               { Token Sym_plus_plus _ _ }
"--"               { Token Sym_dash_dash _ _ }
"+="               { Token Sym_plus_eq _ _ }
"-="               { Token Sym_dash_eq _ _ }
"*="               { Token Sym_aster_eq _ _ }
"/="               { Token Sym_slash_eq _ _ }
"%="               { Token Sym_percent_eq _ _ }
"&="               { Token Sym_amp_eq _ _ }
"|="               { Token Sym_bar_eq _ _ }
"^="               { Token Sym_hat_eq _ _ }
"+:"               { Token Sym_plus_colon _ _ }
"-:"               { Token Sym_dash_colon _ _ }
"::"               { Token Sym_colon_colon _ _ }
".*"               { Token Sym_dot_aster _ _ }
"->"               { Token Sym_dash_gt _ _ }
":="               { Token Sym_colon_eq _ _ }
":/"               { Token Sym_colon_slash _ _ }
"##"               { Token Sym_pound_pound _ _ }
"[*"               { Token Sym_brack_l_aster _ _ }
"[="               { Token Sym_brack_l_eq _ _ }
"=>"               { Token Sym_eq_gt _ _ }
"@*"               { Token Sym_at_aster _ _ }
"(*"               { Token Sym_paren_l_aster _ _ }
"*)"               { Token Sym_aster_paren_r _ _ }
"*>"               { Token Sym_aster_gt _ _ }
"==="              { Token Sym_eq_eq_eq _ _ }
"!=="              { Token Sym_bang_eq_eq _ _ }
"==?"              { Token Sym_eq_eq_question _ _ }
"!=?"              { Token Sym_bang_eq_question _ _ }
">>>"              { Token Sym_gt_gt_gt _ _ }
"<<<"              { Token Sym_lt_lt_lt _ _ }
"<<="              { Token Sym_lt_lt_eq _ _ }
">>="              { Token Sym_gt_gt_eq _ _ }
"<->"              { Token Sym_lt_dash_gt _ _ }
"|->"              { Token Sym_bar_dash_gt _ _ }
"|=>"              { Token Sym_bar_eq_gt _ _ }
"[->"              { Token Sym_brack_l_dash_gt _ _ }
"#-#"              { Token Sym_pound_dash_pound _ _ }
"#=#"              { Token Sym_pound_eq_pound _ _ }
"@@("              { Token Sym_at_at_paren_l _ _ }
"(*)"              { Token Sym_paren_l_aster_paren_r _ _ }
"->>"              { Token Sym_dash_gt_gt _ _ }
"&&&"              { Token Sym_amp_amp_amp _ _ }
"<<<="             { Token Sym_lt_lt_lt_eq _ _ }
">>>="             { Token Sym_gt_gt_gt_eq _ _ }


-- operator precedences, from *lowest* to *highest*
%nonassoc NoElse
%nonassoc "else"
%right  "|->" "|=>" "#-#" "#=#"
%right "iff"
%left "or"
%left "and"
%left "intersect"
%left "within"
%right "throughout"
%left "##"
%nonassoc "[*]" "[=]" "[->]"
%right "->" "<->"
%right "?" ":"
%left  "||"
%left  "&&"
%left  "|" "~|"
%left  "^" "^~" "~^"
%left  "&" "~&"
%left  "==" "!=" "===" "!==" "==?" "!=?"
%left  "<" "<=" ">" ">=" "inside" "dist"
%left  "<<" ">>" "<<<" ">>>"
%left  "+" "-"
%left  "*" "/" "%"
%left  "**"
%right REDUCE_OP "!" "~" "++" "--"
%left  "(" ")" "[" "]" "." "'" "::"

%%

opt(p) :: { Maybe a }
  : p { Just $1 }
  |   { Nothing }

Descriptions :: { [Description] }
  : {- empty -}              { [] }
  | Descriptions ";"         { $1 }
  | Descriptions Description { $1 ++ $2 }

Description :: { [Description] }
  : Part(ModuleKW   , "endmodule"   ) { [$1] }
  | Part(InterfaceKW, "endinterface") { [$1] }
  | PackageDeclaration                { [$1] }
  | PackageItem { map PackageItem $1 }

Type :: { Type }
  : TypeNonIdent { $1 }
  |                 Identifier Dimensions { Alias (Nothing) $1 $2 }
  | Identifier "::" Identifier Dimensions { Alias (Just $1) $3 $4 }
TypeNonIdent :: { Type }
  : PartialType OptSigning Dimensions { $1 $2 $3 }
PartialType :: { Signing -> [Range] -> Type }
  : NetType                                 { \Unspecified ->        Net           $1    }
  | IntegerVectorType                       {                        IntegerVector $1    }
  | IntegerAtomType                         { \sg          -> \[] -> IntegerAtom   $1 sg }
  | NonIntegerType                          { \Unspecified -> \[] -> NonInteger    $1    }
  | "enum" EnumBaseType "{" EnumItems   "}" { \Unspecified -> Enum   $2 $4 }
  | "struct" Packing    "{" StructItems "}" { \Unspecified -> Struct $2 $4 }
  | "union"  Packing    "{" StructItems "}" { \Unspecified -> Union  $2 $4 }
CastingType :: { Type }
  : IntegerVectorType { IntegerVector $1 Unspecified [] }
  | IntegerAtomType   { IntegerAtom   $1 Unspecified    }
  | NonIntegerType    { NonInteger    $1                }
  | Signing           { Implicit      $1             [] }
EnumBaseType :: { Maybe Type }
  : opt(Type) { $1 }
  | DimensionsNonEmpty { Just $ Implicit Unspecified $1 }

Signing :: { Signing }
  : "signed"   { Signed   }
  | "unsigned" { Unsigned }
OptSigning :: { Signing }
  : Signing { $1 }
  | {- empty -} { Unspecified }

NetType :: { NetType }
  : "supply0"   { TSupply0   }
  | "supply1"   { TSupply1   }
  | "tri"       { TTri       }
  | "triand"    { TTriand    }
  | "trior"     { TTrior     }
  | "trireg"    { TTrireg    }
  | "tri0"      { TTri0      }
  | "tri1"      { TTri1      }
  | "uwire"     { TUwire     }
  | "wire"      { TWire      }
  | "wand"      { TWand      }
  | "wor"       { TWor       }
IntegerVectorType :: { IntegerVectorType }
  : "bit"       { TBit       }
  | "logic"     { TLogic     }
  | "reg"       { TReg       }
IntegerAtomType :: { IntegerAtomType }
  : "byte"      { TByte      }
  | "shortint"  { TShortint  }
  | "int"       { TInt       }
  | "longint"   { TLongint   }
  | "integer"   { TInteger   }
  | "time"      { TTime      }
NonIntegerType :: { NonIntegerType }
  : "shortreal" { TShortreal }
  | "real"      { TReal      }
  | "realtime"  { TRealtime  }

EnumItems :: { [(Identifier, Maybe Expr)] }
  : VariablePortIdentifiers { $1 }

StructItems :: { [(Type, Identifier)] }
  : StructItem             { $1 }
  | StructItems StructItem { $1 ++ $2 }
StructItem :: { [(Type, Identifier)] }
  : Type Identifiers ";" { map (\a -> ($1, a)) $2 }

Packing :: { Packing }
  : "packed" OptSigning { Packed $2 }
  | {- empty -}         { Unpacked }

Part(begin, end) :: { Description }
  :          begin opt(Lifetime) Identifier PackageImportDeclarations Params PortDecls ";" ModuleItems end opt(Tag) { Part False $1 $2 $3 (fst $6) ($4 ++ $5 ++ (snd $6) ++ $8) }
  | "extern" begin opt(Lifetime) Identifier PackageImportDeclarations Params PortDecls ";"                          { Part True  $2 $3 $4 (fst $7) ($5 ++ $6 ++ (snd $7)      ) }

ModuleKW :: { PartKW }
  : "module" { Module }
InterfaceKW :: { PartKW }
  : "interface" { Interface }

PackageDeclaration :: { Description }
  : "package" opt(Lifetime) Identifier ";" PackageItems "endpackage" opt(Tag) { Package $2 $3 $5 }

Tag :: { Identifier }
  : ":" Identifier { $2 }

PackageImportDeclarations :: { [ModuleItem] }
  : PackageImportDeclaration PackageImportDeclarations { $1 ++ $2 }
  | {- empty -}                                        { [] }

PackageImportDeclaration :: { [ModuleItem] }
  : "import" PackageImportItems ";" { map (MIPackageItem . uncurry Import) $2 }

Params :: { [ModuleItem] }
  : {- empty -}          { [] }
  | "#" "(" ParamsFollow { map (MIPackageItem . Decl) $3 }
ParamsFollow :: { [Decl] }
  : ParameterDecl(")")              { $1 }
  | ParameterDecl(",") ParamsFollow { $1 ++ $2 }

PortDecls :: { ([Identifier], [ModuleItem]) }
  : "(" DeclTokens(")") { parseDTsAsPortDecls $2 }
  | "("            ")"  { ([], []) }
  | {- empty -}         { ([], []) }

ModportItems :: { [(Identifier, [ModportDecl])] }
  : ModportItem                  { [$1] }
  | ModportItems "," ModportItem { $1 ++ [$3] }
ModportItem :: { (Identifier, [ModportDecl]) }
  : Identifier "(" ModportPortsDeclarations { ($1, $3) }
ModportPortsDeclarations :: { [ModportDecl] }
  : ModportPortsDeclaration(")")                          { $1 }
  | ModportPortsDeclaration(",") ModportPortsDeclarations { $1 ++ $2 }
ModportPortsDeclaration(delim) :: { [ModportDecl] }
  : ModportSimplePortsDeclaration(delim) { $1 }
ModportSimplePortsDeclaration(delim) :: { [ModportDecl] }
  : Direction ModportSimplePorts delim { map (\(a, b) -> ($1, a, b)) $2 }
ModportSimplePorts :: { [(Identifier, Maybe Expr)] }
  : ModportSimplePort                        { [$1] }
  | ModportSimplePorts "," ModportSimplePort { $1 ++ [$3] }
ModportSimplePort :: { (Identifier, Maybe Expr) }
  : "." Identifier "(" opt(Expr) ")" { ($2, $4) }
  | Identifier                       { ($1, Just $ Ident $1) }

Identifier :: { Identifier }
  : simpleIdentifier  { tokenString $1 }
  | escapedIdentifier { tokenString $1 }
  | systemIdentifier  { tokenString $1 }

Identifiers :: { [Identifier] }
  :                 Identifier { [$1] }
  | Identifiers "," Identifier { $1 ++ [$3] }

-- uses delimiter propagation hack to avoid conflicts
DeclTokens(delim) :: { [DeclToken] }
  : DeclToken                  delim  { [$1] }
  | DeclToken       DeclTokens(delim) { [$1] ++ $2 }
  | AsgnOp Expr "," DeclTokens(delim) { [DTAsgn $1 $2, DTComma] ++ $4 }
  | AsgnOp Expr                delim  { [DTAsgn $1 $2] }
DeclToken :: { DeclToken }
  : DeclOrStmtToken   { $1 }
  | ParameterBindings { DTParams   $1 }
  | PortBindings      { DTInstance $1 }

DeclOrStmtTokens(delim) :: { [DeclToken] }
  : DeclOrStmtToken                  delim  { [$1] }
  | DeclOrStmtToken DeclOrStmtTokens(delim) { [$1] ++ $2 }
  | AsgnOp Expr "," DeclOrStmtTokens(delim) { [DTAsgn $1 $2, DTComma] ++ $4 }
  | AsgnOp Expr                      delim  { [DTAsgn $1 $2] }
  | "<=" opt(DelayOrEventControl) Expr "," DeclOrStmtTokens(delim) { [DTAsgnNBlk $2 $3, DTComma] ++ $5 }
  | "<=" opt(DelayOrEventControl) Expr                      delim  { [DTAsgnNBlk $2 $3] }
DeclOrStmtToken :: { DeclToken }
  : ","            { DTComma }
  | PartSelect     { DTRange    $1 }
  | Identifier     { DTIdent    $1 }
  | Direction      { DTDir      $1 }
  | "[" Expr "]"   { DTBit      $2 }
  | LHSConcat      { DTConcat   $1 }
  | PartialType    { DTType     $1 }
  | "." Identifier { DTDot      $2 }
  | Signing        { DTSigning  $1 }
  | Lifetime       { DTLifetime $1 }
  | Identifier "::" Identifier { DTPSIdent $1 $3 }
  | "{" StreamOp StreamSize Concat "}" { DTStream $2 $3           (map toLHS $4) }
  | "{" StreamOp            Concat "}" { DTStream $2 (Number "1") (map toLHS $3) }

VariablePortIdentifiers :: { [(Identifier, Maybe Expr)] }
  : VariablePortIdentifier                             { [$1] }
  | VariablePortIdentifiers "," VariablePortIdentifier { $1 ++ [$3] }
VariablePortIdentifier :: { (Identifier, Maybe Expr) }
  : Identifier          { ($1, Nothing) }
  | Identifier "=" Expr { ($1, Just $3) }

Direction :: { Direction }
  : "inout"  { Inout  }
  | "input"  { Input  }
  | "output" { Output }

ModuleItems :: { [ModuleItem] }
  : {- empty -}            { [] }
  | ModuleItems ModuleItem { $1 ++ $2 }
  | ModuleItems ";"        { $1 }

ModuleItem :: { [ModuleItem] }
  : NonGenerateModuleItem { $1 }
  | ConditionalGenerateConstruct      { [Generate [$1]] }
  | LoopGenerateConstruct             { [Generate [$1]] }
  | "generate" GenItems "endgenerate" { [Generate $2] }
NonGenerateModuleItem :: { [ModuleItem] }
  -- This item covers module instantiations and all declarations
  : DeclTokens(";")                      { parseDTsAsModuleItems $1 }
  | ParameterDecl(";")                   { map (MIPackageItem . Decl) $1 }
  | "defparam" DefparamAsgns ";"         { map (uncurry Defparam) $2 }
  | "assign" opt(DelayControl) LHS "=" Expr ";" { [Assign $2 $3 $5] }
  | AlwaysKW Stmt                        { [AlwaysC $1 $2] }
  | "initial" Stmt                       { [Initial $2] }
  | "genvar" Identifiers ";"             { map Genvar $2 }
  | "modport" ModportItems ";"           { map (uncurry Modport) $2 }
  | NonDeclPackageItem                   { map MIPackageItem $1 }
  | NInputGateKW  NInputGates  ";"       { map (\(a, b, c) -> NInputGate  $1 a b c) $2 }
  | NOutputGateKW NOutputGates ";"       { map (\(a, b, c) -> NOutputGate $1 a b c) $2 }
  | AttributeInstance ModuleItem         { map (MIAttr $1) $2 }
  | AssertionItem                        { [AssertionItem $1] }

-- for ModuleItem, for now
AssertionItem :: { AssertionItem }
  : ConcurrentAssertionItem { $1 }

-- for Stmt, for now
ProceduralAssertionStatement :: { Assertion }
  : ConcurrentAssertionStatement { $1 }
  | ImmediateAssertionStatement  { $1 }

ConcurrentAssertionItem :: { AssertionItem }
  : Identifier ":" ConcurrentAssertionStatement { (Just $1, $3) }
  |                ConcurrentAssertionStatement { (Nothing, $1) }
ConcurrentAssertionStatement :: { Assertion }
  : "assert" "property" "(" PropertySpec ")" ActionBlock { Assert (Left $4) $6 }
  | "assume" "property" "(" PropertySpec ")" ActionBlock { Assume (Left $4) $6 }
  | "cover"  "property" "(" PropertySpec ")" Stmt        { Cover  (Left $4) $6 }

ImmediateAssertionStatement :: { Assertion }
  : SimpleImmediateAssertionStatement { $1 }
SimpleImmediateAssertionStatement :: { Assertion }
  : "assert" "(" Expr ")" ActionBlock { Assert (Right $3) $5 }
  | "assume" "(" Expr ")" ActionBlock { Assume (Right $3) $5 }
  | "cover"  "(" Expr ")" Stmt        { Cover  (Right $3) $5 }

PropertySpec :: { PropertySpec }
  : opt(ClockingEvent) "disable" "iff" "(" Expr ")" PropExpr { PropertySpec $1 (Just $5) $7 }
  | opt(ClockingEvent)                              PropExpr { PropertySpec $1 (Nothing) $2 }

PropExpr :: { PropExpr }
  : SeqExpr { PropExpr $1 }
  | PropExprParens { $1 }
PropExprParens :: { PropExpr }
  : "(" PropExprParens ")" { $2 }
  | SeqExpr "|->" PropExpr { PropExprImpliesO  $1 $3 }
  | SeqExpr "|=>" PropExpr { PropExprImpliesNO $1 $3 }
  | SeqExpr "#-#" PropExpr { PropExprFollowsO  $1 $3 }
  | SeqExpr "#=#" PropExpr { PropExprFollowsNO $1 $3 }
  | PropExpr "iff" PropExpr { PropExprIff $1 $3 }
SeqExpr :: { SeqExpr }
  : Expr { SeqExpr $1 }
  | SeqExprParens { $1 }
SeqExprParens :: { SeqExpr }
  : "(" SeqExprParens ")" { $2 }
  | SeqExpr "and"        SeqExpr { SeqExprAnd        $1 $3 }
  | SeqExpr "or"         SeqExpr { SeqExprOr         $1 $3 }
  | SeqExpr "intersect"  SeqExpr { SeqExprIntersect  $1 $3 }
  | Expr    "throughout" SeqExpr { SeqExprThroughout $1 $3 }
  | SeqExpr "within"     SeqExpr { SeqExprWithin     $1 $3 }
  | SeqExpr "##" Number  SeqExpr { SeqExprDelay (Just $1) (Number $3) $4 }
  |         "##" Number  SeqExpr { SeqExprDelay (Nothing) (Number $2) $3 }
  | "first_match" "(" SeqExpr SeqMatchItems ")" { SeqExprFirstMatch $3 $4 }
SeqMatchItems :: { [SeqMatchItem] }
  : "," SeqMatchItem               { [$2] }
  | SeqMatchItems "," SeqMatchItem { $1 ++ [$3] }
SeqMatchItem :: { SeqMatchItem }
  : ForStepAssignment           { Left $1 }
  | Identifier "(" CallArgs ")" { Right ($1, $3) }

ActionBlock :: { ActionBlock }
  : Stmt %prec NoElse { ActionBlockIf   $1 }
  |      "else" Stmt  { ActionBlockElse (Nothing) $2 }
  | Stmt "else" Stmt  { ActionBlockElse (Just $1) $3 }

AttributeInstance :: { Attr }
  : "(*" AttrSpecs "*)" { Attr $2 }
AttrSpecs :: { [AttrSpec] }
  : AttrSpec               { [$1] }
  | AttrSpecs "," AttrSpec { $1 ++ [$3] }
AttrSpec :: { AttrSpec }
  : Identifier "=" Expr { ($1, Just $3) }
  | Identifier          { ($1, Nothing) }

NInputGates :: { [(Maybe Identifier, LHS, [Expr])] }
  : NInputGate                 { [$1] }
  | NInputGates "," NInputGate { $1 ++ [$3]}
NOutputGates :: { [(Maybe Identifier, [LHS], Expr)] }
  : NOutputGate                  { [$1] }
  | NOutputGates "," NOutputGate { $1 ++ [$3]}

NInputGate :: { (Maybe Identifier, LHS, [Expr]) }
  : opt(Identifier) "(" LHS "," Exprs ")" { ($1, $3, $5) }
NOutputGate :: { (Maybe Identifier, [LHS], Expr) }
  : opt(Identifier) "(" NOutputGateItems { ($1, fst $3, snd $3) }
NOutputGateItems :: { ([LHS], Expr) }
  : Expr ")" { ([], $1) }
  | Expr "," NOutputGateItems { (fst $3 ++ [toLHS $1], snd $3) }

NInputGateKW :: { NInputGateKW }
  : "and"  { GateAnd  }
  | "nand" { GateNand }
  | "or"   { GateOr   }
  | "nor"  { GateNor  }
  | "xor"  { GateXor  }
  | "xnor" { GateXnor }
NOutputGateKW :: { NOutputGateKW }
  : "buf"  { GateBuf  }
  | "not"  { GateNot  }

DefparamAsgns :: { [(LHS, Expr)] }
  : DefparamAsgn                   { [$1] }
  | DefparamAsgns "," DefparamAsgn { $1 ++ [$3] }
DefparamAsgn :: { (LHS, Expr) }
  : LHS "=" Expr { ($1, $3) }

PackageItems :: { [PackageItem] }
  : {- empty -}              { [] }
  | PackageItems PackageItem { $1 ++ $2 }
PackageItem :: { [PackageItem] }
  : DeclTokens(";")    { map Decl $ parseDTsAsDecls $1 }
  | ParameterDecl(";") { map Decl $1 }
  | NonDeclPackageItem { $1 }
NonDeclPackageItem :: { [PackageItem] }
  : "typedef" Type Identifier ";" { [Typedef $2 $3] }
  | "function" opt(Lifetime) FuncRetAndName    TFItems DeclsAndStmts "endfunction" opt(Tag) { [Function $2 (fst $3) (snd $3) (map defaultFuncInput $ (map makeInput $4) ++ fst $5) (snd $5)] }
  | "function" opt(Lifetime) "void" Identifier TFItems DeclsAndStmts "endfunction" opt(Tag) { [Task     $2 $4                (map defaultFuncInput $ $5 ++ fst $6) (snd $6)] }
  | "task"     opt(Lifetime) Identifier        TFItems DeclsAndStmts "endtask"     opt(Tag) { [Task     $2 $3                (map defaultFuncInput $ $4 ++ fst $5) (snd $5)] }
  | "import" PackageImportItems ";" { map (uncurry Import) $2 }
  | "export" PackageImportItems ";" { map (Export .  Just) $2 }
  | "export" "*" "::" "*" ";"       { [Export Nothing] } -- "Nothing" being no restrictions
  | ForwardTypedef ";"              { $1 }
  | TimeunitsDeclaration            { $1 }
ForwardTypedef :: { [PackageItem] }
  : "typedef" "enum"   Identifier { [] }
  | "typedef" "struct" Identifier { [] }
  | "typedef" "union"  Identifier { [] }
TimeunitsDeclaration :: { [PackageItem] }
  : "timeunit" Time          ";" { [] }
  | "timeunit" Time "/" Time ";" { [] }
  | "timeprecision" Time     ";" { [] }

PackageImportItems :: { [(Identifier, Maybe Identifier)] }
  : PackageImportItem                        { [$1] }
  | PackageImportItems "," PackageImportItem { $1 ++ [$3] }
PackageImportItem :: { (Identifier, Maybe Identifier) }
  : Identifier "::" Identifier { ($1, Just $3) }
  | Identifier "::" "*"        { ($1, Nothing) }

FuncRetAndName :: { (Type, Identifier) }
  : Type                       Identifier { ($1                     , $2) }
  |                            Identifier { (Implicit Unspecified [], $1) }
  | Signing                    Identifier { (Implicit $1          [], $2) }
  |         DimensionsNonEmpty Identifier { (Implicit Unspecified $1, $2) }
  | Signing DimensionsNonEmpty Identifier { (Implicit $1          $2, $3) }

AlwaysKW :: { AlwaysKW }
  : "always"       { Always      }
  | "always_comb"  { AlwaysComb  }
  | "always_ff"    { AlwaysFF    }
  | "always_latch" { AlwaysLatch }

Lifetime :: { Lifetime }
  : "static"    { Static    }
  | "automatic" { Automatic }

TFItems :: { [Decl] }
  : "(" DeclTokens(")") ";" { parseDTsAsDecls $2 }
  | "("            ")"  ";" { [] }
  |                     ";" { [] }

ParamType :: { Type }
  : PartialType OptSigning Dimensions { $1 $2 $3 }
  | DimensionsNonEmpty { Implicit Unspecified $1 }
  | Signing Dimensions { Implicit $1          $2 }

Dimensions :: { [Range] }
  : {- empty -}        { [] }
  | DimensionsNonEmpty { $1 }
DimensionsNonEmpty :: { [Range] }
  : Dimension                    { [$1] }
  | DimensionsNonEmpty Dimension { $1 ++ [$2] }
Dimension :: { Range }
  : Range        { $1 }
  | "[" Expr "]" { (simplify $  BinOp Sub $2 (Number "1"), Number "0") }

DeclAsgns :: { [(Identifier, Expr, [Range])] }
  : DeclAsgn               { [$1] }
  | DeclAsgns "," DeclAsgn { $1 ++ [$3] }
DeclAsgn :: { (Identifier, Expr, [Range]) }
  : Identifier                    "=" Expr { ($1, $3, []) }
  | Identifier DimensionsNonEmpty "=" Expr { ($1, $4, $2) }

Range :: { Range }
  : "[" Expr ":"  Expr "]" { ($2, $4) }

PartSelect :: { (PartSelectMode, Range) }
  : "[" Expr ":"  Expr "]" { (NonIndexed  , ($2, $4)) }
  | "[" Expr "+:" Expr "]" { (IndexedPlus , ($2, $4)) }
  | "[" Expr "-:" Expr "]" { (IndexedMinus, ($2, $4)) }

LHS :: { LHS }
  : Identifier         { LHSIdent  $1    }
  | LHS PartSelect     { LHSRange  $1 (fst $2) (snd $2) }
  | LHS "[" Expr "]"   { LHSBit    $1 $3 }
  | LHS "." Identifier { LHSDot    $1 $3 }
  | LHSConcat          { LHSConcat $1    }
  | "{" StreamOp StreamSize Concat "}" { LHSStream $2 $3           (map toLHS $4) }
  | "{" StreamOp            Concat "}" { LHSStream $2 (Number "1") (map toLHS $3) }

LHSConcat :: { [LHS] }
  : "{" LHSs "}" { $2 }
LHSs :: { [LHS] }
  : LHS           { [$1] }
  | LHSs "," LHS  { $1 ++ [$3] }

PortBindings :: { [PortBinding] }
  : "("                    ")" { [] }
  | "(" PortBindingsInside ")" { $2 }
PortBindingsInside :: { [PortBinding] }
  : PortBinding                        { [$1] }
  | PortBinding "," PortBindingsInside { $1 : $3}
PortBinding :: { PortBinding }
  : "." Identifier "(" opt(Expr) ")" { ($2, $4) }
  | "." Identifier                   { ($2, Just $ Ident $2) }
  | Expr                             { ("", Just $1) }
  | ".*"                             { ("*", Nothing) }

ParameterBindings :: { [ParamBinding] }
  : "#" "(" ParamBindingsInside ")" { $3 }
ParamBindingsInside :: { [ParamBinding] }
  : ParamBinding                         { [$1] }
  | ParamBinding "," ParamBindingsInside { $1 : $3}
ParamBinding :: { ParamBinding }
  : "." Identifier "(" TypeOrExpr ")" { ($2, $4) }
  | "." Identifier "("            ")" { ($2, Right Nil) }
  | TypeOrExpr                        { ("", $1) }

Stmts :: { [Stmt] }
  : {- empty -} { [] }
  | Stmts Stmt  { $1 ++ [$2] }

Stmt :: { Stmt }
  : StmtNonAsgn         { $1 }
  | LHS AsgnOp Expr ";" { AsgnBlk $2 $1 $3 }
  |                 Identifier ";" { Subroutine (Nothing) $1 (Args [] []) }
  | Identifier "::" Identifier ";" { Subroutine (Just $1) $3 (Args [] []) }
  | LHS "<=" opt(DelayOrEventControl) Expr ";" { Asgn $3 $1 $4 }
  | LHS IncOrDecOperator ";" { AsgnBlk (AsgnOp $2) $1 (Number "1") }
  | IncOrDecOperator LHS ";" { AsgnBlk (AsgnOp $1) $2 (Number "1") }
StmtNonAsgn :: { Stmt }
  : ";" { Null }
  | "begin" opt(Tag) DeclsAndStmts "end" opt(Tag) { Block (combineTags $2 $5) (fst $3) (snd $3) }
  | Unique "if" "(" Expr ")" Stmt "else" Stmt         { If $1 $4 $6 $8        }
  | Unique "if" "(" Expr ")" Stmt %prec NoElse        { If $1 $4 $6 Null      }
  | "for" "("            ";"  opt(Expr) ";" ForStep ")" Stmt { For []                           $4 $6 $8 }
  | "for" "(" DeclTokens(";") opt(Expr) ";" ForStep ")" Stmt { For (parseDTsAsDeclsAndAsgns $3) $4 $6 $8 }
  | Unique CaseKW "(" Expr ")" Cases opt(CaseDefault) "endcase" { Case $1 $2 $4 $6 $7 }
  |                 Identifier "(" CallArgs ")" ";" { Subroutine (Nothing) $1 $3 }
  | Identifier "::" Identifier "(" CallArgs ")" ";" { Subroutine (Just $1) $3 $5 }
  | TimingControl Stmt                         { Timing $1 $2 }
  | "return" Expr ";"                          { Return $2 }
  | "while"  "(" Expr ")" Stmt                 { While   $3 $5 }
  | "repeat" "(" Expr ")" Stmt                 { RepeatL $3 $5 }
  | "do"      Stmt "while" "(" Expr ")" ";"    { DoWhile $5 $2 }
  | "forever" Stmt                             { Forever $2 }
  | "->" Identifier ";"                        { Trigger $2 }
  | AttributeInstance Stmt                     { StmtAttr $1 $2 }
  | ProceduralAssertionStatement               { Assertion $1 }

Unique :: { Maybe UniquePriority }
  : {- empty -} { Nothing }
  | "unique"    { Just Unique   }
  | "unique0"   { Just Unique0  }
  | "priority"  { Just Priority }

ForStep :: { [(LHS, AsgnOp, Expr)] }
  : {- empty -}     { [] }
  | ForStepNonEmpty { $1 }
ForStepNonEmpty :: { [(LHS, AsgnOp, Expr)] }
  : ForStepAssignment                     { [$1] }
  | ForStepNonEmpty "," ForStepAssignment { $1 ++ [$3] }
ForStepAssignment :: { (LHS, AsgnOp, Expr) }
  : LHS AsgnOp Expr { ($1, $2, $3) }
  | IncOrDecOperator LHS { ($2, AsgnOp $1, Number "1") }
  | LHS IncOrDecOperator { ($1, AsgnOp $2, Number "1") }

DeclsAndStmts :: { ([Decl], [Stmt]) }
  : DeclOrStmt DeclsAndStmts { combineDeclsAndStmts $1 $2 }
  | StmtNonAsgn Stmts        { ([], $1 : $2) }
  | {- empty -}              { ([], []) }
DeclOrStmt :: { ([Decl], [Stmt]) }
  : DeclOrStmtTokens(";") { parseDTsAsDeclOrAsgn $1 }
  | ParameterDecl(";")    { ($1, []) }

ParameterDecl(delim) :: { [Decl] }
  : ParameterDeclKW                                       DeclAsgns delim { makeParamDecls $1 (Implicit Unspecified []) $2 }
  | ParameterDeclKW                             ParamType DeclAsgns delim { makeParamDecls $1 ($2                     ) $3 }
  | ParameterDeclKW                 Identifier Dimensions DeclAsgns delim { makeParamDecls $1 (Alias (Nothing)   $2 $3) $4 }
  | ParameterDeclKW Identifier "::" Identifier Dimensions DeclAsgns delim { makeParamDecls $1 (Alias (Just $2)   $4 $5) $6 }
  | ParameterDeclKW "type"                                TypeAsgns delim { map (uncurry $ ParamType $1) $3 }
ParameterDeclKW :: { ParamScope }
  : "parameter"  { Parameter  }
  | "localparam" { Localparam }

TypeAsgns :: { [(Identifier, Maybe Type)] }
  : TypeAsgn               { [$1] }
  | TypeAsgns "," TypeAsgn { $1 ++ [$3] }
TypeAsgn :: { (Identifier, Maybe Type) }
  : Identifier "=" Type { ($1, Just $3) }
  | Identifier          { ($1, Nothing) }

-- TODO: This does not allow for @identifier
ClockingEvent :: { Sense }
  : "@" "(" Senses ")" { $3 }

TimingControl :: { Timing }
  : DelayOrEventControl { $1 }
  | CycleDelay          { Cycle $1 }
DelayOrEventControl :: { Timing }
  : DelayControl { Delay $1 }
  | EventControl { Event $1 }
DelayControl :: { Expr }
  : "#" DelayValue   { $2 }
  | "#" "(" Expr ")" { $3 }
CycleDelay :: { Expr }
  : "##" Expr { $2 }
EventControl :: { Sense }
  : "@" "(" Senses ")" { $3 }
  | "@" "(*)"          { SenseStar }
  | "@*"               { SenseStar }
Senses :: { Sense }
  : Sense             { $1 }
  | Senses "or" Sense { SenseOr $1 $3 }
  | Senses ","  Sense { SenseOr $1 $3 }
Sense :: { Sense }
  :           LHS { Sense        $1 }
  | "posedge" LHS { SensePosedge $2 }
  | "negedge" LHS { SenseNegedge $2 }

DelayValue :: { Expr }
  : Number { Number $1 }
-- TODO: Support these other DelayValues?
-- | real_number
-- | ps_identifier
-- | time_literal
-- | 1step

CaseKW :: { CaseKW }
  : "case"  { CaseN }
  | "casex" { CaseX }
  | "casez" { CaseZ }

Cases :: { [Case] }
  : {- empty -}  { [] }
  | Cases Case   { $1 ++ [$2] }

Case :: { Case }
  : Exprs ":" Stmt { ($1, $3) }

CaseDefault :: { Stmt }
  : "default" opt(":") Stmt { $3 }

Number :: { String }
  : number    { tokenString $1 }

String :: { String }
  : string { tail $ init $ tokenString $1 }

Time :: { String }
  : time { tokenString $1 }

CallArgs :: { Args }
  : {- empty -}                        { Args [            ] [] }
  |                NamedCallArgsFollow { Args [            ] $1 }
  | Expr                 NamedCallArgs { Args [Just $1     ] $2 }
  |      UnnamedCallArgs NamedCallArgs { Args (Nothing : $1) $2 }
  | Expr UnnamedCallArgs NamedCallArgs { Args (Just $1 : $2) $3 }
UnnamedCallArgs :: { [Maybe Expr] }
  : "," opt(Expr)                { [$2] }
  | UnnamedCallArgs "," opt(Expr) { $1 ++ [$3] }
NamedCallArgs :: { [(Identifier, Maybe Expr)] }
  : {- empty -}        { [] }
  | "," NamedCallArgsFollow  { $2 }
NamedCallArgsFollow :: { [(Identifier, Maybe Expr)] }
  : NamedCallArg                         { [$1] }
  | NamedCallArgsFollow "," NamedCallArg { $1 ++ [$3] }
NamedCallArg :: { (Identifier, Maybe Expr) }
  : "." Identifier "(" opt(Expr) ")" { ($2, $4) }

Exprs :: { [Expr] }
  :           Expr  { [$1] }
  | Exprs "," Expr  { $1 ++ [$3] }

TypeOrExpr :: { TypeOrExpr }
  : TypeNonIdent { Left $1 }
  | Expr         { Right $1 }

Expr :: { Expr }
  : "(" Expr ")"                { $2 }
  | String                      { String $1 }
  | Number                      { Number $1 }
  |                 Identifier "(" CallArgs ")" { Call (Nothing) $1 $3 }
  | Identifier "::" Identifier "(" CallArgs ")" { Call (Just $1) $3 $5 }
  | DimsFn "(" TypeOrExpr ")"   { DimsFn $1 $3 }
  | DimFn  "(" TypeOrExpr ")"   { DimFn  $1 $3 (Number "1") }
  | DimFn  "(" TypeOrExpr "," Expr ")" { DimFn $1 $3 $5 }
  | Identifier                  { Ident $1 }
  | Identifier "::" Identifier  { PSIdent $1 $3 }
  | Expr PartSelect             { Range $1 (fst $2) (snd $2) }
  | Expr "[" Expr "]"           { Bit   $1 $3 }
  | "{" Expr Concat "}"         { Repeat $2 $3 }
  | Concat                      { Concat $1 }
  | Expr "?" Expr ":" Expr      { Mux $1 $3 $5 }
  | CastingType "'" "(" Expr ")" { Cast (Left            $1) $4 }
  | Number      "'" "(" Expr ")" { Cast (Right $ Number  $1) $4 }
  |                 Identifier  "'" "(" Expr ")" { Cast (Right $ Ident   $1   ) $4 }
  | Identifier "::" Identifier  "'" "(" Expr ")" { Cast (Right $ PSIdent $1 $3) $6 }
  | Expr "." Identifier         { Dot $1 $3 }
  | "'" "{" PatternItems "}"    { Pattern $3 }
  | "{" StreamOp StreamSize Concat "}" { Stream $2 $3           $4 }
  | "{" StreamOp            Concat "}" { Stream $2 (Number "1") $3 }
  | Expr "inside" Concat        { foldl1 (BinOp LogOr) $ map (BinOp Eq $1) $3 }
  -- binary expressions
  | Expr "||"  Expr { BinOp LogOr  $1 $3 }
  | Expr "&&"  Expr { BinOp LogAnd $1 $3 }
  | Expr "->"  Expr { BinOp LogImp $1 $3 }
  | Expr "<->" Expr { BinOp LogEq  $1 $3 }
  | Expr "|"   Expr { BinOp BitOr  $1 $3 }
  | Expr "^"   Expr { BinOp BitXor $1 $3 }
  | Expr "&"   Expr { BinOp BitAnd $1 $3 }
  | Expr "~^"  Expr { BinOp BitXnor $1 $3 }
  | Expr "^~"  Expr { BinOp BitXnor $1 $3 }
  | Expr "+"   Expr { BinOp Add $1 $3 }
  | Expr "-"   Expr { BinOp Sub $1 $3 }
  | Expr "*"   Expr { BinOp Mul $1 $3 }
  | Expr "/"   Expr { BinOp Div $1 $3 }
  | Expr "%"   Expr { BinOp Mod $1 $3 }
  | Expr "**"  Expr { BinOp Pow $1 $3 }
  | Expr "=="  Expr { BinOp Eq $1 $3 }
  | Expr "!="  Expr { BinOp Ne $1 $3 }
  | Expr "<"   Expr { BinOp Lt $1 $3 }
  | Expr "<="  Expr { BinOp Le $1 $3 }
  | Expr ">"   Expr { BinOp Gt $1 $3 }
  | Expr ">="  Expr { BinOp Ge $1 $3 }
  | Expr "===" Expr { BinOp TEq $1 $3 }
  | Expr "!==" Expr { BinOp TNe $1 $3 }
  | Expr "==?" Expr { BinOp WEq $1 $3 }
  | Expr "!=?" Expr { BinOp WNe $1 $3 }
  | Expr "<<"  Expr { BinOp ShiftL $1 $3 }
  | Expr ">>"  Expr { BinOp ShiftR $1 $3 }
  | Expr "<<<" Expr { BinOp ShiftAL $1 $3 }
  | Expr ">>>" Expr { BinOp ShiftAR $1 $3 }
  -- unary expressions
  | "!"  Expr                 { UniOp LogNot $2 }
  | "~"  Expr                 { UniOp BitNot $2 }
  | "+"  Expr %prec REDUCE_OP { UniOp UniAdd $2 }
  | "-"  Expr %prec REDUCE_OP { UniOp UniSub $2 }
  | "&"  Expr %prec REDUCE_OP { UniOp RedAnd  $2 }
  | "~&" Expr %prec REDUCE_OP { UniOp RedNand $2 }
  | "|"  Expr %prec REDUCE_OP { UniOp RedOr   $2 }
  | "~|" Expr %prec REDUCE_OP { UniOp RedNor  $2 }
  | "^"  Expr %prec REDUCE_OP { UniOp RedXor  $2 }
  | "~^" Expr %prec REDUCE_OP { UniOp RedXnor $2 }
  | "^~" Expr %prec REDUCE_OP { UniOp RedXnor $2 }

PatternItems :: { [(Maybe Identifier, Expr)] }
  : PatternNamedItems   { map (\(x,e) -> (Just x, e)) $1 }
  | PatternUnnamedItems { zip (repeat Nothing) $1 }
PatternNamedItems :: { [(Identifier, Expr)] }
  : PatternNamedItem                       { [$1] }
  | PatternNamedItems "," PatternNamedItem { $1 ++ [$3] }
PatternNamedItem :: { (Identifier, Expr) }
  : Identifier ":" Expr { ($1, $3) }
  | "default"  ":" Expr { (tokenString $1, $3) }
PatternUnnamedItems :: { [Expr] }
  : Exprs { $1 }

Concat :: { [Expr] }
  : "{" Exprs "}" { $2 }

StreamOp :: { StreamOp }
  : "<<" { StreamL }
  | ">>" { StreamR }
StreamSize :: { Expr }
  : TypeNonIdent { DimsFn FnBits (Left $1) }
  | Expr         { $1 }

GenItemOrNull :: { GenItem }
  : GenItem { $1 }
  | ";"     { GenNull }

GenItems :: { [GenItem] }
  : {- empty -}      { [] }
  | GenItems GenItem { $1 ++ [$2] }

GenItem :: { GenItem }
  : GenBlock              { uncurry GenBlock $1 }
  | NonGenerateModuleItem { genItemsToGenItem $ map GenModuleItem $1 }
  | ConditionalGenerateConstruct { $1 }
  | LoopGenerateConstruct        { $1 }
ConditionalGenerateConstruct :: { GenItem }
  : "if" "(" Expr ")" GenItemOrNull "else" GenItemOrNull { GenIf $3 $5 $7      }
  | "if" "(" Expr ")" GenItemOrNull %prec NoElse         { GenIf $3 $5 GenNull }
  | "case" "(" Expr ")" GenCases opt(GenCaseDefault) "endcase" { GenCase $3 $5 $6 }
LoopGenerateConstruct :: { GenItem }
  : "for" "(" GenvarInitialization ";" Expr ";" GenvarIteration ")" GenBlock { (uncurry $ GenFor $3 $5 $7) $9 }

GenBlock :: { (Maybe Identifier, [GenItem]) }
  : "begin" opt(Tag) GenItems "end" opt(Tag) { (combineTags $2 $5, $3) }

GenCases :: { [GenCase] }
  : {- empty -}      { [] }
  | GenCases GenCase { $1 ++ [$2] }

GenCase :: { GenCase }
  : Exprs ":" GenItemOrNull { ($1, $3) }

GenCaseDefault :: { GenItem }
  : "default" opt(":") GenItemOrNull { $3 }

GenvarInitialization :: { (Bool, Identifier, Expr) }
  : "genvar" Identifier "=" Expr { (True , $2, $4) }
  |          Identifier "=" Expr { (False, $1, $3) }

GenvarIteration :: { (Identifier, AsgnOp, Expr) }
  : Identifier AsgnOp Expr { ($1, $2, $3) }
  | IncOrDecOperator Identifier { ($2, AsgnOp $1, Number "1") }
  | Identifier IncOrDecOperator { ($1, AsgnOp $2, Number "1") }

AsgnOp :: { AsgnOp }
  : "="    { AsgnOpEq }
  | "+="   { AsgnOp Add }
  | "-="   { AsgnOp Sub }
  | "*="   { AsgnOp Mul }
  | "/="   { AsgnOp Div }
  | "%="   { AsgnOp Mod }
  | "&="   { AsgnOp BitAnd }
  | "|="   { AsgnOp BitOr  }
  | "^="   { AsgnOp BitXor }
  | "<<="  { AsgnOp ShiftL }
  | ">>="  { AsgnOp ShiftR }
  | "<<<=" { AsgnOp ShiftAL }
  | ">>>=" { AsgnOp ShiftAR }

IncOrDecOperator :: { BinOp }
  : "++" { Add }
  | "--" { Sub }

DimsFn :: { DimsFn }
  : "$bits"                { FnBits               }
  | "$dimensions"          { FnDimensions         }
  | "$unpacked_dimensions" { FnUnpackedDimensions }
DimFn :: { DimFn }
  : "$left"                { FnLeft               }
  | "$right"               { FnRight              }
  | "$low"                 { FnLow                }
  | "$high"                { FnHigh               }
  | "$increment"           { FnIncrement          }
  | "$size"                { FnSize               }

{

parseError :: [Token] -> a
parseError a = case a of
  []              -> error "Parse error: no tokens left to parse."
  Token t s p : _ -> error $ "Parse error: unexpected token '" ++ s ++ "' (" ++ show t ++ ") at " ++ show p ++ "."

genItemsToGenItem :: [GenItem] -> GenItem
genItemsToGenItem [x] = x
genItemsToGenItem xs = GenBlock Nothing xs

combineDeclsAndStmts :: ([Decl], [Stmt]) -> ([Decl], [Stmt]) -> ([Decl], [Stmt])
combineDeclsAndStmts (a1, b1) (a2, b2) = (a1 ++ a2, b1 ++ b2)

makeInput :: Decl -> Decl
makeInput (Variable _ t x a me) = Variable Input t x a me
makeInput other = error $ "unexpected non-var decl: " ++ (show other)

defaultFuncInput :: Decl -> Decl
defaultFuncInput (Variable dir (Implicit sg rs) x a me) =
  Variable dir t x a me
  where
    t = if dir == Input || dir == Inout
      then IntegerVector TLogic sg rs
      else Implicit sg rs
defaultFuncInput other = other

combineTags :: Maybe Identifier -> Maybe Identifier -> Maybe Identifier
combineTags (Just a) (Just b) =
  if a == b
    then Just a
    else error $ "tag mismatch: " ++ show (a, b)
combineTags Nothing other = other
combineTags other   _     = other

toLHS :: Expr -> LHS
toLHS expr =
  case exprToLHS expr of
    Just lhs -> lhs
    Nothing -> error $ "Parse error: cannot convert expression to LHS: "
                ++ show expr

makeParamDecls
  :: ParamScope
  -> Type
  -> [(Identifier, Expr, [Range])]
  -> [Decl]
makeParamDecls s t items =
  map mapper items
  where
    (tf, rs) = typeRanges t
    mapper (x, e, a) = Param s (tf $ a ++ rs) x e

}
