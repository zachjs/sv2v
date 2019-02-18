{
module Language.SystemVerilog.Parser.Parse (modules) where

import Data.Bits
import Data.List

import Data.Maybe
import Language.SystemVerilog.AST
import Language.SystemVerilog.Parser.Tokens
}

%name modules
%tokentype { Token }
%error { parseError }

%expect 0

%token

"always"           { Token KW_always       _ _ }
"always_comb"      { Token KW_always_comb  _ _ }
"always_ff"        { Token KW_always_ff    _ _ }
"always_latch"     { Token KW_always_latch _ _ }
"assign"           { Token KW_assign     _ _ }
"begin"            { Token KW_begin      _ _ }
"case"             { Token KW_case       _ _ }
"casez"            { Token KW_casez      _ _ }
"default"          { Token KW_default    _ _ }
"else"             { Token KW_else       _ _ }
"end"              { Token KW_end        _ _ }
"endcase"          { Token KW_endcase    _ _ }
"endfunction"      { Token KW_endfunction _ _ }
"endgenerate"      { Token KW_endgenerate _ _ }
"endmodule"        { Token KW_endmodule  _ _ }
"function"         { Token KW_function   _ _ }
"for"              { Token KW_for        _ _ }
"generate"         { Token KW_generate   _ _ }
"genvar"           { Token KW_genvar     _ _ }
"if"               { Token KW_if         _ _ }
"initial"          { Token KW_initial    _ _ }
"inout"            { Token KW_inout      _ _ }
"input"            { Token KW_input      _ _ }
"integer"          { Token KW_integer    _ _ }
"localparam"       { Token KW_localparam _ _ }
"logic"            { Token KW_logic      _ _ }
"module"           { Token KW_module     _ _ }
"negedge"          { Token KW_negedge    _ _ }
"or"               { Token KW_or         _ _ }
"output"           { Token KW_output     _ _ }
"parameter"        { Token KW_parameter  _ _ }
"posedge"          { Token KW_posedge    _ _ }
"reg"              { Token KW_reg        _ _ }
"wire"             { Token KW_wire       _ _ }

simpleIdentifier   { Token Id_simple     _ _ }
escapedIdentifier  { Token Id_escaped    _ _ }
systemIdentifier   { Token Id_system     _ _ }
number             { Token Lit_number    _ _ }
string             { Token Lit_string    _ _ }

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
"=?="              { Token Sym_eq_question_eq _ _ }
"!?="              { Token Sym_bang_question_eq _ _ }
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

%nonassoc NoElse
%nonassoc "else"
%right "?" ":"
%left  "||"
%left  "&&"
%left  "|" "~|"
%left  "^" "^~"
%left  "&" "~&"
%left  "==" "!=" "===" "!=="
%left  "<" "<=" ">" ">="
%left  "<<" ">>" "<<<" ">>>"
%left  "+" "-"
%left  "*" "/" "%"
%left  UPlus UMinus "!" "~" RedOps


%%

opt(p) :: { Maybe a }
  : p { Just $1 }
  |   { Nothing }

Modules :: { [Module] }
  : {- empty -}    { [] }
  | Modules Module { $1 ++ [$2] }

Module :: { Module }
  : "module" Identifier Params           ";" ModuleItems "endmodule" opt(";") { Module $2 [] ($3 ++ $5) }
  | "module" Identifier Params PortNames ";" ModuleItems "endmodule" opt(";") { Module $2 $4 ($3 ++ $6) }
  | "module" Identifier Params PortDecls ";" ModuleItems "endmodule" opt(";") { Module $2 (getPortNames $4) ($3 ++ $4 ++ $6) }

Params :: { [ModuleItem] }
  : {- empty -}        { [] }
  | "#" "(" ParamDecls { $3 }
ParamDecls :: { [ModuleItem] }
  : ParamDecl(")")            { $1 }
  | ParamDecl(",") ParamDecls { $1 ++ $2 }
ParamDecl(delim) :: { [ModuleItem] }
  : "parameter" opt(Range) DeclAsgns delim { map (MIParameter . (uncurry $ Parameter $2)) $3 }

Identifier :: { Identifier }
  : simpleIdentifier  { tokenString $1 }
  | escapedIdentifier { tokenString $1 }
  | systemIdentifier  { tokenString $1 }

Identifiers :: { [Identifier] }
  :                 Identifier { [$1] }
  | Identifiers "," Identifier { $1 ++ [$3] }

PortNames :: { [Identifier] }
  : "(" Identifiers ")" { $2 }

-- abuses delimiter propogation hack to avoid conflicts
PortDecls :: { [ModuleItem] }
  : "(" PortDeclsFollow { $2 }
PortDeclsFollow :: { [ModuleItem] }
  : ")"                           { [] }
  | PortDecl(")")                 { $1 }
  | PortDecl(",") PortDeclsFollow { $1 ++ $2 }

PortDecl(delim) :: { [ModuleItem] }
  : "inout"  opt(NetType) opt(Range) Identifiers             delim { portDeclToModuleItems Inout  $2 $3 (zip $4 (repeat Nothing)) }
  | "input"  opt(NetType) opt(Range) Identifiers             delim { portDeclToModuleItems Input  $2 $3 (zip $4 (repeat Nothing)) }
  | "output" "wire"       opt(Range) Identifiers             delim { portDeclToModuleItems Output (Just Wire ) $3 (zip $4 (repeat Nothing)) }
  | "output" "reg"        opt(Range) VariablePortIdentifiers delim { portDeclToModuleItems Output (Just Reg  ) $3 $4 }
  | "output" "logic"      opt(Range) VariablePortIdentifiers delim { portDeclToModuleItems Output (Just Logic) $3 $4 }
NetType :: { Maybe Range -> Type }
  : "wire"  { Wire }
  | "logic" { Logic }
VariablePortIdentifiers :: { [(Identifier, Maybe Expr)] }
  : VariablePortIdentifier                             { [$1] }
  | VariablePortIdentifiers "," VariablePortIdentifier { $1 ++ [$3] }
VariablePortIdentifier :: { (Identifier, Maybe Expr) }
  : Identifier          { ($1, Nothing) }
  | Identifier "=" Expr { ($1, Just $3) }

ModuleItems :: { [ModuleItem] }
  : {- empty -}            { [] }
  | ModuleItems ModuleItem { $1 ++ $2 }

ModuleItem :: { [ModuleItem] }
  : PortDecl(";")                                        { $1 }
  | "reg"    opt(Range) VariableIdentifiers ";"          { map (uncurry $ LocalNet $ Reg   $2) $3 }
  | "wire"   opt(Range) VariableIdentifiers ";"          { map (uncurry $ LocalNet $ Wire  $2) $3 }
  | "logic"  opt(Range) VariableIdentifiers ";"          { map (uncurry $ LocalNet $ Logic $2) $3 }
  | ParameterDeclaration                                 { map MIParameter  $1 }
  | LocalparamDeclaration                                { map MILocalparam $1 }
  | IntegerDeclaration                                   { map MIIntegerV   $1 }
  | "assign" LHS "=" Expr ";"                            { [Assign $2 $4] }
  | AlwaysKW Stmt                                        { [AlwaysC $1 $2] }
  | Identifier ParameterBindings ModuleInstantiations ";" { map (uncurry $ Instance $1 $2) $3 }
  | "function" opt(RangeOrType) Identifier FunctionItems Stmt "endfunction" { [Function $2 $3 $4 $5] }
  | "genvar" Identifiers ";"                             { map Genvar $2 }
  | "generate" GenItems "endgenerate"                    { [Generate $2] }

AlwaysKW :: { AlwaysKW }
  : "always"       { Always      }
  | "always_comb"  { AlwaysComb  }
  | "always_ff"    { AlwaysFF    }
  | "always_latch" { AlwaysLatch }

ModuleInstantiations :: { [(Identifier, [PortBinding])] }
  : ModuleInstantiation                          { [$1] }
  | ModuleInstantiations "," ModuleInstantiation { $1 ++ [$3] }
ModuleInstantiation :: { (Identifier, [PortBinding]) }
  : Identifier "(" Bindings ")"  { ($1, $3) }

FunctionItems :: { [(Bool, BlockItemDeclaration)] }
  : "(" FunctionPortList ";" BlockItemDeclarations { (map ((,) True) $2) ++ (map ((,) False) $4) }
  | "(" FunctionPortList ";"                       { (map ((,) True) $2) }
  | ";" FunctionItemDeclarations                   { $2 }
FunctionPortList :: { [BlockItemDeclaration] }
  : FunctionInputDeclaration(")")                  { $1 }
  | FunctionInputDeclaration(",") FunctionPortList { $1 ++ $2 }
FunctionItemDeclarations :: { [(Bool, BlockItemDeclaration)] }
  : FunctionItemDeclaration                          { $1 }
  | FunctionItemDeclarations FunctionItemDeclaration { $1 ++ $2 }
FunctionItemDeclaration :: { [(Bool, BlockItemDeclaration)] }
  : BlockItemDeclaration          { map ((,) False) $1 }
  | FunctionInputDeclaration(";") { map ((,) True ) $1 }
FunctionInputDeclaration(delim) :: { [BlockItemDeclaration] }
  : "input" opt("reg") opt(Range) Identifiers delim { map (\x -> BIDReg $3 x []) $4 }
  | "input" "integer"             Identifiers delim { map (\x -> BIDIntegerV $ IntegerV x $ Left []) $3 }

ParameterDeclaration :: { [Parameter] }
  : "parameter"  opt(Range) DeclAsgns ";" { map (uncurry $ Parameter  $2) $3 }

LocalparamDeclaration :: { [Localparam] }
  : "localparam" opt(Range) DeclAsgns ";" { map (uncurry $ Localparam $2) $3 }

IntegerDeclaration :: { [IntegerV] }
  : "integer" VariableIdentifiers ";" { map (uncurry IntegerV) $2 }

RangeOrType :: { Either Range () }
  : Range     { Left $1 }
  | "integer" { Right () }

EventControl :: { Sense }
  : "@" "(" Sense ")" { $3 }
  | "@" "(" "*"   ")" { SenseStar }
  | "@" "*"           { SenseStar }
  | "@*"              { SenseStar }

VariableIdentifiers :: { [(Identifier, Either [Range] (Maybe Expr))] }
  : VariableType                         { [$1] }
  | VariableIdentifiers "," VariableType { $1 ++ [$3] }
VariableType :: { (Identifier, Either [Range] (Maybe Expr)) }
  : Identifier            { ($1, Right $ Nothing) }
  | Identifier "=" Expr   { ($1, Right $ Just $3) }
  | Identifier Dimensions { ($1, Left $2) }

Dimensions :: { [Range] }
  : Range            { [$1] }
  | Dimensions Range { $1 ++ [$2] }

DeclAsgns :: { [(Identifier, Expr)] }
  : DeclAsgn               { [$1] }
  | DeclAsgns "," DeclAsgn { $1 ++ [$3] }
DeclAsgn :: { (Identifier, Expr) }
  : Identifier "=" Expr { ($1, $3) }

Range :: { Range }
  : "[" Expr ":" Expr "]"  { ($2, $4) }

LHS :: { LHS }
: Identifier              { LHS       $1    }
| Identifier Range        { LHSRange  $1 $2 }
| Identifier "[" Expr "]" { LHSBit    $1 $3 }
| "{" LHSs "}"            { LHSConcat $2 }

LHSs :: { [LHS] }
:          LHS  { [$1] }
| LHSs "," LHS  { $1 ++ [$3] }

Sense :: { Sense }
:            Sense1 { $1 }
| Sense "or" Sense1 { SenseOr $1 $3 }

Sense1 :: { Sense }
:           LHS { Sense        $1 }
| "posedge" LHS { SensePosedge $2 }
| "negedge" LHS { SenseNegedge $2 }

Bindings :: { [(Identifier, Maybe Expr)] }
  : {- empty -}      { [] }
  | BindingsNonEmpty { $1 }
BindingsNonEmpty :: { [(Identifier, Maybe Expr)] }
  : Binding                      { [$1] }
  | Binding "," BindingsNonEmpty { $1 : $3}
Binding :: { (Identifier, Maybe Expr) }
  : "." Identifier "(" MaybeExpr ")" { ($2, $4) }
  | "." Identifier                   { ($2, Just $ Ident $2) }
  | Expr                             { ("", Just $1) }

ParameterBindings :: { [(Identifier, Maybe Expr)] }
  : {- empty -}                  { [] }
  | "#" "(" BindingsNonEmpty ")" { $3 }

Stmts :: { [Stmt] }
  : {- empty -} { [] }
  | Stmts Stmt  { $1 ++ [$2] }

Stmt :: { Stmt }
  : ";" { Null }
  | "begin"                                      Stmts "end" { Block Nothing         $2 }
  | "begin" ":" Identifier                       Stmts "end" { Block (Just ($3, [])) $4 }
  | "begin" ":" Identifier BlockItemDeclarations Stmts "end" { Block (Just ($3, $4)) $5 }
  | "if" "(" Expr ")" Stmt "else" Stmt               { If $3 $5 $7        }
  | "if" "(" Expr ")" Stmt %prec NoElse              { If $3 $5 Null      }
  | "for" "(" Identifier "=" Expr ";" Expr ";" Identifier "=" Expr ")" Stmt { For ($3, $5) $7 ($9, $11) $13 }
  | LHS "=" Expr ";"                                 { BlockingAssignment $1 $3 }
  | LHS "<=" Expr ";"                                { NonBlockingAssignment $1 $3 }
  | "case" "(" Expr ")" Cases opt(CaseDefault) "endcase" { Case $3 $5 $6 }
  | EventControl Stmt                                { Timing $1 $2 }

BlockItemDeclarations :: { [BlockItemDeclaration] }
  : BlockItemDeclaration                       { $1 }
  | BlockItemDeclarations BlockItemDeclaration { $1 ++ $2 }

BlockItemDeclaration :: { [BlockItemDeclaration] }
  : "reg" opt(Range) BlockVariableIdentifiers ";" { map (uncurry $ BIDReg $2) $3 }
  | ParameterDeclaration                          { map BIDParameter  $1 }
  | LocalparamDeclaration                         { map BIDLocalparam $1 }
  | IntegerDeclaration                            { map BIDIntegerV   $1 }
BlockVariableIdentifiers :: { [(Identifier, [Range])] }
  : BlockVariableType                              { [$1] }
  | BlockVariableIdentifiers "," BlockVariableType { $1 ++ [$3] }
BlockVariableType :: { (Identifier, [Range]) }
  : Identifier            { ($1, []) }
  | Identifier Dimensions { ($1, $2) }

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
: string    { toString $1 }

CallArgs :: { [Expr] }
  :              Expr  { [$1] }
  | CallArgs "," Expr  { $1 ++ [$3] }

MaybeExpr :: { Maybe Expr }
:         { Nothing }
| Expr    { Just $1 }

Exprs :: { [Expr] }
:           Expr  { [$1] }
| Exprs "," Expr  { $1 ++ [$3] }

Expr :: { Expr }
: "(" Expr ")"                { $2 }
| String                      { String $1 }
| Number                      { Number $1 }
| Identifier "(" CallArgs ")" { Call $1 $3 }
| Identifier                  { Ident      $1    }
| Identifier Range            { IdentRange $1 $2 }
| Identifier "[" Expr "]"     { IdentBit   $1 $3 }
| "{" Expr "{" Exprs "}" "}"  { Repeat $2 $4 }
| "{" Exprs "}"               { Concat $2 }
| Expr "?" Expr ":" Expr      { Mux $1 $3 $5 }
| Expr "||" Expr              { BinOp Or  $1 $3 }
| Expr "&&" Expr              { BinOp And $1 $3 }
| Expr "|"  Expr              { BinOp BWOr $1 $3 }
| Expr "^"  Expr              { BinOp BWXor $1 $3 }
| Expr "&"  Expr              { BinOp BWAnd $1 $3 }
| Expr "==" Expr              { BinOp Eq $1 $3 }
| Expr "!=" Expr              { BinOp Ne $1 $3 }
| Expr "<"  Expr              { BinOp Lt $1 $3 }
| Expr "<=" Expr              { BinOp Le $1 $3 }
| Expr ">"  Expr              { BinOp Gt $1 $3 }
| Expr ">=" Expr              { BinOp Ge $1 $3 }
| Expr "<<" Expr              { BinOp ShiftL $1 $3 }
| Expr ">>" Expr              { BinOp ShiftR $1 $3 }
| Expr "+"  Expr              { BinOp Add $1 $3 }
| Expr "-"  Expr              { BinOp Sub $1 $3 }
| Expr "*"  Expr              { BinOp Mul $1 $3 }
| Expr "/"  Expr              { BinOp Div $1 $3 }
| Expr "%"  Expr              { BinOp Mod $1 $3 }
| Expr "<<<" Expr             { BinOp ShiftAL $1 $3 }
| Expr ">>>" Expr             { BinOp ShiftAR $1 $3 }
| "!" Expr                    { UniOp Not $2 }
| "~" Expr                    { UniOp BWNot $2 }
| "+" Expr %prec UPlus        { UniOp UAdd $2 }
| "-" Expr %prec UMinus       { UniOp USub $2 }
| "&"  Expr %prec RedOps      { UniOp RedAnd  $2 }
| "~&" Expr %prec RedOps      { UniOp RedNand $2 }
| "|"  Expr %prec RedOps      { UniOp RedOr   $2 }
| "~|" Expr %prec RedOps      { UniOp RedNor  $2 }
| "^"  Expr %prec RedOps      { UniOp RedXor  $2 }
| "~^" Expr %prec RedOps      { UniOp RedXnor $2 }
| "^~" Expr %prec RedOps      { UniOp RedXnor $2 }

GenItemOrNull :: { GenItem }
  : GenItem { $1 }
  | ";"     { GenNull }

GenItems :: { [GenItem] }
  : {- empty -}      { [] }
  | GenItems GenItem { $1 ++ [$2] }

GenItem :: { GenItem }
  : "if" "(" Expr ")" GenItemOrNull "else" GenItemOrNull { GenIf $3 $5 $7      }
  | "if" "(" Expr ")" GenItemOrNull %prec NoElse         { GenIf $3 $5 GenNull }
  | "begin"                GenItems "end"                { GenBlock Nothing   $2 }
  | "begin" ":" Identifier GenItems "end"                { GenBlock (Just $3) $4 }
  | "case" "(" Expr ")" GenCases opt(GenCaseDefault) "endcase" { GenCase $3 $5 $6 }
  | "for" "(" Identifier "=" Expr ";" Expr ";" Identifier "=" Expr ")" "begin" ":" Identifier GenItems "end" { GenFor ($3, $5) $7 ($9, $11) $15 $16 }
  -- TODO: We should restrict it to the module items that are actually allowed.
  | ModuleItem { genItemsToGenItem $ map GenModuleItem $1 }

GenCases :: { [GenCase] }
  : {- empty -}      { [] }
  | GenCases GenCase { $1 ++ [$2] }

GenCase :: { GenCase }
  : Exprs ":" GenItemOrNull { ($1, $3) }

GenCaseDefault :: { GenItem }
  : "default" opt(":") GenItemOrNull { $3 }


{
parseError :: [Token] -> a
parseError a = case a of
  []              -> error "Parse error: no tokens left to parse."
  Token t s p : _ -> error $ "Parse error: unexpected token '" ++ s ++ "' (" ++ show t ++ ") at " ++ show p ++ "."

toString :: Token -> String
toString = tail . init . tokenString

portDeclToModuleItems
  :: Direction
  -> (Maybe ((Maybe Range) -> Type))
  -> Maybe Range
  -> [(Identifier, Maybe Expr)]
  -> [ModuleItem]
portDeclToModuleItems dir Nothing mr l =
  map (PortDecl dir mr) $ map toIdentifier $ l
  where
    toIdentifier (x, Just  _) = error "ParseError: Incomplete port decl cannot have initialization"
    toIdentifier (x, Nothing) = x
portDeclToModuleItems dir (Just tf) mr l =
  concat $ map toItems l
  where
    toItems (x, e) =
      [ PortDecl dir mr x
      , LocalNet (tf mr) x (Right e) ]

getPortNames :: [ModuleItem] -> [Identifier]
getPortNames items =
  mapMaybe getPortName items
  where
    getPortName :: ModuleItem -> Maybe Identifier
    getPortName (PortDecl _ _ ident) = Just ident
    getPortName _ = Nothing

stmtsToStmt :: [Stmt] -> Stmt
stmtsToStmt [] = error "stmtsToStmt given empty list!"
stmtsToStmt [s] = s
stmtsToStmt ss = Block Nothing ss

moduleItemsToSingleGenItem :: [ModuleItem] -> GenItem
moduleItemsToSingleGenItem [x] = GenModuleItem x
moduleItemsToSingleGenItem other = error $ "multiple module items in a generate block where only one was allowed" ++ show other

genItemsToGenItem :: [GenItem] -> GenItem
genItemsToGenItem [] = error "genItemsToGenItem given empty list!"
genItemsToGenItem [x] = x
genItemsToGenItem xs = GenBlock Nothing xs

}

