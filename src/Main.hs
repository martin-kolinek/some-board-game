import           Control.Monad
import           CssClass
import           Reflex
import           Reflex.Dom
import           Style

main :: IO ()
main = mainWidgetWithCss mainStyleByteString $
  forM_ [1..15] $ const card

card = divCssClass cardWrapperClass $
        divCssClass cardClass $
          text "hello world"
