module Util.Prettyprint (render, sepBy, sepMap) where

import Prettyprinter
import Prettyprinter.Render.String (renderString)

render :: Doc ann -> String
render =
  renderString
    . layoutPretty
      ( defaultLayoutOptions {layoutPageWidth = AvailablePerLine 50 1.0}
      )

sepBy :: Doc a -> [Doc a] -> Doc a
sepBy s xs = case xs of
  [] -> mempty
  _ -> concatWith (\x acc -> x <> s <> acc) xs

sepMap :: Doc a -> (b -> Doc a) -> [b] -> Doc a
sepMap s f = sepBy s . map f
