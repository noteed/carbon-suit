module Text.CarbonSuit.Processing where

import Text.CarbonSuit.Types

-- Merges consecutive Prompt blocks into one block.
mergePromptBlocks :: Carbon -> Carbon
mergePromptBlocks (Carbon fn bs) = Carbon fn (go bs)
  where go (Prompt p1 : Prompt p2 : rest) = go $ Prompt (p1 ++ [""] ++ p2) : rest
        go (b : rest) = b : go rest
        go [] = []
