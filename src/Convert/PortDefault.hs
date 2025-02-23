{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for input ports with default values
 -
 - The default values are permitted to depend on complex constants declared
 - within the instantiated module. The relevant constants are copied into the
 - site of the instantiation.
 -}

module Convert.PortDefault (convert) where

import Control.Monad.Writer.Strict
import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)

import Convert.Traverse
import Convert.UnbasedUnsized (inlineConstants)
import Language.SystemVerilog.AST

type Part = ([ModuleItem], [PortDefault])
type Parts = Map.Map Identifier Part
type PortDefault = (Identifier, Decl)

convert :: [AST] -> [AST]
convert files =
    map (traverseDescriptions convertDescription) files'
    where
        (files', parts) = runWriter $
            mapM (traverseDescriptionsM preparePart) files
        convertDescription = traverseModuleItems $ convertModuleItem parts

-- remove and record input ports with defaults
preparePart :: Description -> Writer Parts Description
preparePart description@(Part att ext kw lif name ports items) =
    if null portDefaults
        then return description
        else do
            tell $ Map.singleton name (items, portDefaults)
            return $ Part att ext kw lif name ports items'
    where
        (items', portDefaults) = runWriter $
            mapM (traverseNestedModuleItemsM prepareModuleItem) items
preparePart other = return other

prepareModuleItem :: ModuleItem -> Writer [PortDefault] ModuleItem
prepareModuleItem (MIPackageItem (Decl decl)) =
    prepareDecl decl <&> MIPackageItem . Decl
prepareModuleItem other = return other

prepareDecl :: Decl -> Writer [PortDefault] Decl
prepareDecl (Variable Input t x [] e) | e /= Nil =
    preparePortDefault t x e >> return (Variable Input t x [] Nil)
prepareDecl (Net Input n s t x [] e) | e /= Nil =
    preparePortDefault t x e >> return (Net Input n s t x [] Nil)
prepareDecl other = return other

preparePortDefault :: Type -> Identifier -> Expr -> Writer [PortDefault] ()
preparePortDefault t x e = tell [(x, decl)]
    where
        decl = Param Localparam t' x e
        t' = case t of
            Implicit sg [] -> Implicit sg [(RawNum 0, RawNum 0)]
            _ -> t

-- add default port bindings to module instances that need them
convertModuleItem :: Parts -> ModuleItem -> ModuleItem
convertModuleItem parts (Instance moduleName params instanceName ds bindings) =
    if isNothing maybePart || null neededDecls
        then instanceBase bindings
        else Generate $ map GenModuleItem $
                stubItems ++ [instanceBase $ neededBindings ++ bindings]
    where
        instanceBase = Instance moduleName params instanceName ds
        maybePart = Map.lookup moduleName parts
        Just (moduleItems, portDefaults) = maybePart

        -- determine which defaulted ports are unbound
        (neededBindings, neededDecls) = unzip
            [ ( (port, Ident $ blockName ++ '_' : port)
              , MIPackageItem $ Decl decl
              )
            | (port, decl) <- portDefaults
            , isNothing (lookup port bindings)
            ]

        -- inline and prefix the declarations used by the defaults
        stubItems = inlineConstants blockName params moduleItems neededDecls
        blockName = "sv2v_pd_" ++ instanceName

convertModuleItem _ other = other
