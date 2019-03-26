{
module Language.SystemVerilog.Parser.Parse (descriptions) where

import Data.List
import Data.Maybe

import Language.SystemVerilog.AST
import Language.SystemVerilog.Parser.ParseDecl
import Language.SystemVerilog.Parser.Tokens
}

%name descriptions
%tokentype { Token }
%error { parseError }

%expect 0

%token

"always"           { Token KW_always       _ _ }
"always_comb"      { Token KW_always_comb  _ _ }
"always_ff"        { Token KW_always_ff    _ _ }
"always_latch"     { Token KW_always_latch _ _ }
"and"              { Token KW_and          _ _ }
"assign"           { Token KW_assign       _ _ }
"automatic"        { Token KW_automatic    _ _ }
"begin"            { Token KW_begin        _ _ }
"bit"              { Token KW_bit          _ _ }
"buf"              { Token KW_buf          _ _ }
"byte"             { Token KW_byte         _ _ }
"case"             { Token KW_case         _ _ }
"casex"            { Token KW_casex        _ _ }
"casez"            { Token KW_casez        _ _ }
"default"          { Token KW_default      _ _ }
"defparam"         { Token KW_defparam     _ _ }
"do"               { Token KW_do           _ _ }
"else"             { Token KW_else         _ _ }
"end"              { Token KW_end          _ _ }
"endcase"          { Token KW_endcase      _ _ }
"endfunction"      { Token KW_endfunction  _ _ }
"endgenerate"      { Token KW_endgenerate  _ _ }
"endinterface"     { Token KW_endinterface _ _ }
"endmodule"        { Token KW_endmodule    _ _ }
"endtask"          { Token KW_endtask      _ _ }
"enum"             { Token KW_enum         _ _ }
"extern"           { Token KW_extern       _ _ }
"for"              { Token KW_for          _ _ }
"forever"          { Token KW_forever      _ _ }
"function"         { Token KW_function     _ _ }
"generate"         { Token KW_generate     _ _ }
"genvar"           { Token KW_genvar       _ _ }
"if"               { Token KW_if           _ _ }
"initial"          { Token KW_initial      _ _ }
"inout"            { Token KW_inout        _ _ }
"input"            { Token KW_input        _ _ }
"int"              { Token KW_int          _ _ }
"integer"          { Token KW_integer      _ _ }
"interface"        { Token KW_interface    _ _ }
"localparam"       { Token KW_localparam   _ _ }
"logic"            { Token KW_logic        _ _ }
"longint"          { Token KW_longint      _ _ }
"modport"          { Token KW_modport      _ _ }
"module"           { Token KW_module       _ _ }
"nand"             { Token KW_nand         _ _ }
"negedge"          { Token KW_negedge      _ _ }
"nor"              { Token KW_nor          _ _ }
"not"              { Token KW_not          _ _ }
"or"               { Token KW_or           _ _ }
"output"           { Token KW_output       _ _ }
"packed"           { Token KW_packed       _ _ }
"parameter"        { Token KW_parameter    _ _ }
"posedge"          { Token KW_posedge      _ _ }
"real"             { Token KW_real         _ _ }
"realtime"         { Token KW_realtime     _ _ }
"reg"              { Token KW_reg          _ _ }
"repeat"           { Token KW_repeat       _ _ }
"return"           { Token KW_return       _ _ }
"shortint"         { Token KW_shortint     _ _ }
"shortreal"        { Token KW_shortreal    _ _ }
"signed"           { Token KW_signed       _ _ }
"static"           { Token KW_static       _ _ }
"struct"           { Token KW_struct       _ _ }
"supply0"          { Token KW_supply0      _ _ }
"supply1"          { Token KW_supply1      _ _ }
"task"             { Token KW_task         _ _ }
"time"             { Token KW_time         _ _ }
"tri"              { Token KW_tri          _ _ }
"tri0"             { Token KW_tri0         _ _ }
"tri1"             { Token KW_tri1         _ _ }
"triand"           { Token KW_triand       _ _ }
"trior"            { Token KW_trior        _ _ }
"trireg"           { Token KW_trireg       _ _ }
"typedef"          { Token KW_typedef      _ _ }
"unique"           { Token KW_unique       _ _ }
"unsigned"         { Token KW_unsigned     _ _ }
"uwire"            { Token KW_uwire        _ _ }
"wand"             { Token KW_wand         _ _ }
"while"            { Token KW_while        _ _ }
"wire"             { Token KW_wire         _ _ }
"wor"              { Token KW_wor          _ _ }
"xnor"             { Token KW_xnor         _ _ }
"xor"              { Token KW_xor          _ _ }

simpleIdentifier   { Token Id_simple       _ _ }
escapedIdentifier  { Token Id_escaped      _ _ }
systemIdentifier   { Token Id_system       _ _ }
number             { Token Lit_number      _ _ }
string             { Token Lit_string      _ _ }

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
"|->"              { Token Sym_bar_dash_gt _ _ }
"|=>"              { Token Sym_bar_eq_gt _ _ }
"[->"              { Token Sym_brack_l_dash_gt _ _ }
"@@("              { Token Sym_at_at_paren_l _ _ }
"(*)"              { Token Sym_paren_l_aster_paren_r _ _ }
"->>"              { Token Sym_dash_gt_gt _ _ }
"&&&"              { Token Sym_amp_amp_amp _ _ }
"<<<="             { Token Sym_lt_lt_lt_eq _ _ }
">>>="             { Token Sym_gt_gt_gt_eq _ _ }

directive          { Token Spe_Directive _ _ }

-- operator precedences, from *lowest* to *highest*
%nonassoc NoElse
%nonassoc "else"
%right "?" ":"
%left  "||"
%left  "&&"
%left  "|" "~|"
%left  "^" "^~"
%left  "&" "~&"
%left  "==" "!=" "===" "!==" "==?" "!=?"
%left  "<" "<=" ">" ">="
%left  "<<" ">>" "<<<" ">>>"
%left  "+" "-"
%left  "*" "/" "%"
%left  "**"
%right REDUCE_OP "!" "~" "++" "--"
%left  "(" ")" "[" "]" "."


%%

opt(p) :: { Maybe a }
  : p { Just $1 }
  |   { Nothing }

Descriptions :: { [Description] }
  : {- empty -}              { [] }
  | Descriptions ";"         { $1 }
  | Descriptions Description { $1 ++ [$2] }

Description :: { Description }
  : Part(ModuleKW   , "endmodule"   ) { $1 }
  | Part(InterfaceKW, "endinterface") { $1 }
  | PackageItem { PackageItem $1 }
  | Directive   { Directive $1 }

Directive :: { String }
  : directive { tokenString $1 }

Type :: { Type }
  : PartialType         Dimensions { $1 Unspecified $2 }
  | PartialType Signing Dimensions { $1 $2 $3 }
  | Identifier          Dimensions { Alias $1 $2 }
PartialType :: { Signing -> [Range] -> Type }
  : NetType                                 { \Unspecified ->        Net           $1    }
  | IntegerVectorType                       {                        IntegerVector $1    }
  | IntegerAtomType                         { \sg          -> \[] -> IntegerAtom   $1 sg }
  | NonIntegerType                          { \Unspecified -> \[] -> NonInteger    $1    }
  | "enum"   opt(Type) "{" EnumItems   "}"  { \Unspecified -> Enum   $2 $4 }
  | "struct" Packing   "{" StructItems "}"  { \Unspecified -> Struct $2 $4 }

CastingType :: { Type }
  : IntegerVectorType { IntegerVector $1 Unspecified [] }
  | IntegerAtomType   { IntegerAtom   $1 Unspecified    }
  | NonIntegerType    { NonInteger    $1                }
  | Signing           { Implicit      $1             [] }


Signing :: { Signing }
  : "signed"   { Signed }
  | "unsigned" { Unsigned }

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
  : StructItem             { [$1] }
  | StructItems StructItem { $1 ++ [$2] }
StructItem :: { (Type, Identifier) }
  : Type Identifier ";" { ($1, $2) }

Packing :: { Packing }
  : "packed" Signing { Packed $2 }
  | "packed"         { Packed Unspecified }
  | {- empty -}      { Unpacked }

Part(begin, end) :: { Description }
  :          begin opt(Lifetime) Identifier Params PortDecls ";" ModuleItems end opt(Tag) { Part False $1 $2 $3 (fst $5) ($4 ++ (snd $5) ++ $7) }
  | "extern" begin opt(Lifetime) Identifier Params PortDecls ";"                          { Part True  $2 $3 $4 (fst $6) ($5 ++ (snd $6)      ) }

ModuleKW :: { PartKW }
  : "module" { Module }
InterfaceKW :: { PartKW }
  : "interface" { Interface }

Tag :: { Identifier }
  : ":" Identifier { $2 }

Params :: { [ModuleItem] }
  : {- empty -}        { [] }
  | "#" "(" ParamDecls { $3 }
ParamDecls :: { [ModuleItem] }
  : ParamDecl(")")            { $1 }
  | ParamDecl(",") ParamDecls { $1 ++ $2 }
ParamDecl(delim) :: { [ModuleItem] }
  : "parameter" ParamType DeclAsgns delim { map (MIDecl . (uncurry $ Parameter $2)) $3 }

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
  : DeclOrStmtToken     { $1 }
  | ParameterBindings   { DTParams   $1 }
  | ModuleInstantiation { DTInstance $1 }

DeclOrStmtTokens(delim) :: { [DeclToken] }
  : DeclOrStmtToken                  delim  { [$1] }
  | DeclOrStmtToken DeclOrStmtTokens(delim) { [$1] ++ $2 }
  | AsgnOp Expr "," DeclOrStmtTokens(delim) { [DTAsgn $1 $2, DTComma] ++ $4 }
  | AsgnOp Expr                      delim  { [DTAsgn $1 $2] }
  | "<=" opt(DelayOrEventControl) Expr "," DeclOrStmtTokens(delim) { [DTAsgnNBlk $2 $3, DTComma] ++ $5 }
  | "<=" opt(DelayOrEventControl) Expr                      delim  { [DTAsgnNBlk $2 $3] }
DeclOrStmtToken :: { DeclToken }
  : ","            { DTComma }
  | Range          { DTRange   $1 }
  | Identifier     { DTIdent   $1 }
  | Direction      { DTDir     $1 }
  | "[" Expr "]"   { DTBit     $2 }
  | "{" LHSs "}"   { DTConcat  $2 }
  | PartialType    { DTType    $1 }
  | "." Identifier { DTDot     $2 }
  | Signing        { DTSigning $1 }

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

ModuleItem :: { [ModuleItem] }
  -- This item covers module instantiations and all declarations
  : DeclTokens(";")  { parseDTsAsModuleItems $1 }
  | "parameter"  ParamType DeclAsgns ";" { map MIDecl $ map (uncurry $ Parameter  $2) $3 }
  | "localparam" ParamType DeclAsgns ";" { map MIDecl $ map (uncurry $ Localparam $2) $3 }
  | "defparam" DefparamAsgns ";"         { map (uncurry Defparam) $2 }
  | "assign" opt(DelayControl) LHS "=" Expr ";" { [Assign $2 $3 $5] }
  | AlwaysKW Stmt                        { [AlwaysC $1 $2] }
  | "initial" Stmt                       { [Initial $2] }
  | "genvar" Identifiers ";"             { map Genvar $2 }
  | "generate" GenItems "endgenerate"    { [Generate $2] }
  | "modport" ModportItems ";"           { map (uncurry Modport) $2 }
  | PackageItem                          { [MIPackageItem $1] }
  | NInputGateKW  NInputGates  ";"       { map (\(a, b, c) -> NInputGate  $1 a b c) $2 }
  | NOutputGateKW NOutputGates ";"       { map (\(a, b, c) -> NOutputGate $1 a b c) $2 }
  | AttributeInstance ModuleItem         { map (MIAttr $1) $2 }

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
  | Expr "," NOutputGateItems { (fst $3 ++ [exprToLHS $1], snd $3) }

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

PackageItem :: { PackageItem }
  : "typedef" Type Identifier ";" { Typedef $2 $3 }
  | "function" opt(Lifetime) FuncRetAndName TFItems DeclsAndStmts "endfunction" opt(Tag) { Function $2 (fst $3) (snd $3) (map defaultFuncInput $ (map makeInput $4) ++ fst $5) (snd $5) }
  | "task" opt(Lifetime) Identifier TFItems DeclsAndStmts "endtask" opt(Tag) { Task $2 $3 (map defaultFuncInput $ $4 ++ fst $5) (snd $5) }

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

ModuleInstantiation :: { [PortBinding] }
  : "(" Bindings ")" { $2 }

TFItems :: { [Decl] }
  : "(" DeclTokens(")") ";" { parseDTsAsDecls $2 }
  |                     ";" { [] }

ParamType :: { Type }
  : "integer" Signing  { IntegerAtom TInteger $2 }
  | "integer"          { IntegerAtom TInteger Unspecified }
  |         Dimensions { Implicit Unspecified $1 }
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

DeclAsgns :: { [(Identifier, Expr)] }
  : DeclAsgn               { [$1] }
  | DeclAsgns "," DeclAsgn { $1 ++ [$3] }
DeclAsgn :: { (Identifier, Expr) }
  : Identifier "=" Expr { ($1, $3) }

Range :: { Range }
  : "[" Expr ":" Expr "]"  { ($2, $4) }

LHS :: { LHS }
  : Identifier         { LHSIdent  $1    }
  | LHS Range          { LHSRange  $1 $2 }
  | LHS "[" Expr "]"   { LHSBit    $1 $3 }
  | LHS "." Identifier { LHSDot    $1 $3 }
  | "{" LHSs "}"       { LHSConcat $2    }

LHSs :: { [LHS] }
  : LHS           { [$1] }
  | LHSs "," LHS  { $1 ++ [$3] }

Bindings :: { [(Identifier, Maybe Expr)] }
  : {- empty -}      { [] }
  | BindingsNonEmpty { $1 }
BindingsNonEmpty :: { [(Identifier, Maybe Expr)] }
  : Binding                      { [$1] }
  | Binding "," BindingsNonEmpty { $1 : $3}
Binding :: { (Identifier, Maybe Expr) }
  : "." Identifier "(" opt(Expr) ")" { ($2, $4) }
  | "." Identifier                   { ($2, Just $ Ident $2) }
  | Expr                             { ("", Just $1) }
  | ".*"                             { ("*", Nothing) }

ParameterBindings :: { [(Identifier, Maybe Expr)] }
  : "#" "(" BindingsNonEmpty ")" { $3 }

Stmts :: { [Stmt] }
  : {- empty -} { [] }
  | Stmts Stmt  { $1 ++ [$2] }

Stmt :: { Stmt }
  : StmtNonAsgn         { $1 }
  | LHS AsgnOp Expr ";" { AsgnBlk $2 $1 $3 }
  | Identifier      ";" { Subroutine $1 [] }
  | LHS "<=" opt(DelayOrEventControl) Expr ";" { Asgn $3 $1 $4 }
StmtNonAsgn :: { Stmt }
  : ";" { Null }
  | "begin" opt(Tag) DeclsAndStmts "end" opt(Tag) { Block (combineTags $2 $5) (fst $3) (snd $3) }
  | "if" "(" Expr ")" Stmt "else" Stmt         { If $3 $5 $7        }
  | "if" "(" Expr ")" Stmt %prec NoElse        { If $3 $5 Null      }
  | "for" "(" Identifier "=" Expr ";" Expr ";" Identifier "=" Expr ")" Stmt { For ($3, $5) $7 ($9, $11) $13 }
  | Unique CaseKW "(" Expr ")" Cases opt(CaseDefault) "endcase" { Case $1 $2 $4 $6 $7 }
  | TimingControl Stmt                         { Timing $1 $2 }
  | "return" Expr ";"                          { Return $2 }
  | Identifier "(" CallArgs ")" ";"            { Subroutine $1 $3 }
  | "while"  "(" Expr ")" Stmt                 { While   $3 $5 }
  | "repeat" "(" Expr ")" Stmt                 { RepeatL $3 $5 }
  | "do"      Stmt "while" "(" Expr ")" ";"    { DoWhile $5 $2 }
  | "forever" Stmt                             { Forever $2 }
  | "->" Identifier ";"                        { Trigger $2 }
  | AttributeInstance Stmt                     { StmtAttr $1 $2 }

DeclsAndStmts :: { ([Decl], [Stmt]) }
  : DeclOrStmt DeclsAndStmts { combineDeclsAndStmts $1 $2 }
  | StmtNonAsgn Stmts        { ([], $1 : $2) }
  | {- empty -}              { ([], []) }
DeclOrStmt :: { ([Decl], [Stmt]) }
  : DeclOrStmtTokens(";") { parseDTsAsDeclOrAsgn $1 }
  | "parameter"  ParamType DeclAsgns ";" { (map (uncurry $ Parameter  $2) $3, []) }
  | "localparam" ParamType DeclAsgns ";" { (map (uncurry $ Localparam $2) $3, []) }

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

Unique :: { Bool }
  : "unique"    { True  }
  | {- empty -} { False }

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

CallArgs :: { [Maybe Expr] }
  : {- empty -}         { [] }
  | Expr                { [Just $1] }
  |      CallArgsFollow { (Nothing) : $1 }
  | Expr CallArgsFollow { (Just $1) : $2 }
CallArgsFollow :: { [Maybe Expr] }
  : "," opt(Expr)                { [$2] }
  | "," opt(Expr) CallArgsFollow { $2 : $3 }

Exprs :: { [Expr] }
  :           Expr  { [$1] }
  | Exprs "," Expr  { $1 ++ [$3] }

Expr :: { Expr }
  : "(" Expr ")"                { $2 }
  | String                      { String $1 }
  | Number                      { Number $1 }
  | Identifier "(" CallArgs ")" { Call $1 $3 }
  | Identifier                  { Ident $1 }
  | Expr Range                  { Range $1 $2 }
  | Expr "[" Expr "]"           { Bit   $1 $3 }
  | "{" Expr "{" Exprs "}" "}"  { Repeat $2 $4 }
  | "{" Exprs "}"               { Concat $2 }
  | Expr "?" Expr ":" Expr      { Mux $1 $3 $5 }
  | CastingType "'" "(" Expr ")" { Cast ($1         ) $4 }
  | Identifier  "'" "(" Expr ")" { Cast (Alias $1 []) $4 }
  | Expr "." Identifier         { Dot $1 $3 }
  | "'" "{" PatternItems "}"    { Pattern $3 }
  -- binary expressions
  | Expr "||"  Expr { BinOp LogOr  $1 $3 }
  | Expr "&&"  Expr { BinOp LogAnd $1 $3 }
  | Expr "|"   Expr { BinOp BitOr  $1 $3 }
  | Expr "^"   Expr { BinOp BitXor $1 $3 }
  | Expr "&"   Expr { BinOp BitAnd $1 $3 }
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
PatternUnnamedItems :: { [Expr] }
  : Exprs { $1 }

GenItemOrNull :: { GenItem }
  : GenItem { $1 }
  | ";"     { GenNull }

GenItems :: { [GenItem] }
  : {- empty -}      { [] }
  | GenItems GenItem { $1 ++ [$2] }

GenItem :: { GenItem }
  : "if" "(" Expr ")" GenItemOrNull "else" GenItemOrNull { GenIf $3 $5 $7      }
  | "if" "(" Expr ")" GenItemOrNull %prec NoElse         { GenIf $3 $5 GenNull }
  | GenBlock                                             { uncurry GenBlock $1 }
  | "case" "(" Expr ")" GenCases opt(GenCaseDefault) "endcase" { GenCase $3 $5 $6 }
  | "for" "(" Identifier "=" Expr ";" Expr ";" GenvarIteration ")" GenBlock { (uncurry $ GenFor ($3, $5) $7 $9) $11  }
  -- TODO: We should restrict it to the module items that are actually allowed.
  | ModuleItem { genItemsToGenItem $ map GenModuleItem $1 }

GenBlock :: { (Maybe Identifier, [GenItem]) }
  : "begin" opt(Tag) GenItems "end" opt(Tag) { (combineTags $2 $5, $3) }

GenCases :: { [GenCase] }
  : {- empty -}      { [] }
  | GenCases GenCase { $1 ++ [$2] }

GenCase :: { GenCase }
  : Exprs ":" GenItemOrNull { ($1, $3) }

GenCaseDefault :: { GenItem }
  : "default" opt(":") GenItemOrNull { $3 }

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

{

parseError :: [Token] -> a
parseError a = case a of
  []              -> error "Parse error: no tokens left to parse."
  Token t s p : _ -> error $ "Parse error: unexpected token '" ++ s ++ "' (" ++ show t ++ ") at " ++ show p ++ "."

genItemsToGenItem :: [GenItem] -> GenItem
genItemsToGenItem [] = error "genItemsToGenItem given empty list!"
genItemsToGenItem [x] = x
genItemsToGenItem xs = GenBlock Nothing xs

combineDeclsAndStmts :: ([Decl], [Stmt]) -> ([Decl], [Stmt]) -> ([Decl], [Stmt])
combineDeclsAndStmts (a1, b1) (a2, b2) = (a1 ++ a2, b1 ++ b2)

makeInput :: Decl -> Decl
makeInput (Variable _ t x a me) = Variable Input t x a me
makeInput other = error $ "unexpected non-var decl: " ++ (show other)

defaultFuncInput :: Decl -> Decl
defaultFuncInput (Variable Input (Implicit sg rs) x a me) =
  Variable Input (IntegerVector TLogic sg rs) x a me
defaultFuncInput other = other

combineTags :: Maybe Identifier -> Maybe Identifier -> Maybe Identifier
combineTags (Just a) (Just b) =
  if a == b
    then Just a
    else error $ "tag mismatch: " ++ show (a, b)
combineTags Nothing other = other
combineTags other   _     = other

exprToLHS :: Expr -> LHS
exprToLHS (Ident   x) = LHSIdent x
exprToLHS (Bit   e b) = LHSBit   (exprToLHS e) b
exprToLHS (Range e r) = LHSRange (exprToLHS e) r
exprToLHS (Dot   e x) = LHSDot   (exprToLHS e) x
exprToLHS (Concat es) = LHSConcat (map exprToLHS es)
exprToLHS other =
  error $ "Parse error: cannot convert expression to LHS: " ++ show other

}
