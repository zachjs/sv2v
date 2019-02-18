{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Template converter for ModuleItem transformations
 -
 - Also has coverage for ModuleItems inside of generate blocks
 -}

module Convert.Template.ModuleItem (moduleItemConverter) where

import Data.Maybe
import Language.SystemVerilog.AST

type Converter = ModuleItem -> ModuleItem

moduleItemConverter :: Converter -> (AST -> AST)
moduleItemConverter f = convert f

convert :: Converter -> AST -> AST
convert f modules = map (convertDescription f) modules

convertDescription :: Converter -> Description -> Description
convertDescription f (Module name ports items) =
    Module name ports $ map (convertModuleItem f) items
convertDescription _ (Typedef a b) = Typedef a b

convertModuleItem :: Converter -> ModuleItem -> ModuleItem
convertModuleItem f (Generate items) = f $ Generate $ map (convertGenItem f) items
convertModuleItem f other = f other

convertGenItem :: Converter -> GenItem -> GenItem
convertGenItem f item = convertGenItem' item
    where
        convertGenItem' :: GenItem -> GenItem
        convertGenItem' (GenBlock x items) = GenBlock x $ map convertGenItem' items
        convertGenItem' (GenFor a b c d items) = GenFor a b c d $ map convertGenItem' items
        convertGenItem' (GenIf e i1 i2) = GenIf e (convertGenItem' i1) (convertGenItem' i2)
        convertGenItem' (GenNull) = GenNull
        convertGenItem' (GenModuleItem moduleItem) = GenModuleItem $ f moduleItem
        convertGenItem' (GenCase e cases def) = GenCase e cases' def'
            where
                cases' = zip (map fst cases) (map (convertGenItem' . snd) cases)
                def' = if def == Nothing
                    then Nothing
                    else Just $ convertGenItem' $ fromJust def
