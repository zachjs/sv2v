{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for `edge` sensitivity
 -
 - IEEE 1800-2017 Section 9.4.2 defines `edge` as either `posedge` or `negedge`.
 - This does not convert senses in assertions as they are likely either removed
 - or fully supported downstream.
 -}

module Convert.EventEdge (convert) where

import Convert.Traverse
import Language.SystemVerilog.AST

convert :: [AST] -> [AST]
convert =
    map $ traverseDescriptions $ traverseModuleItems $ traverseStmts $
        traverseNestedStmts convertStmt

convertStmt :: Stmt -> Stmt
convertStmt (Asgn op (Just timing) lhs expr) =
    Asgn op (Just $ convertTiming timing) lhs expr
convertStmt (Timing timing stmt) =
    Timing (convertTiming timing) stmt
convertStmt other = other

convertTiming :: Timing -> Timing
convertTiming (Event event) = Event $ convertEvent event
convertTiming other = other

convertEvent :: Event -> Event
convertEvent EventStar = EventStar
convertEvent (EventExpr e) = EventExpr $ convertEventExpr e

convertEventExpr :: EventExpr -> EventExpr
convertEventExpr (EventExprOr v1 v2) =
    EventExprOr (convertEventExpr v1) (convertEventExpr v2)
convertEventExpr (EventExprEdge Edge lhs) =
    EventExprOr (EventExprEdge Posedge lhs) (EventExprEdge Negedge lhs)
convertEventExpr other@EventExprEdge{} = other
