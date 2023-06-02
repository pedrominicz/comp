module Parse (parse) where

import Expr

import Data.ByteString (ByteString)
import qualified FlatParse.Basic as F

type Parse a = F.Parser () a

assert :: Parse a -> Parse a
assert p = F.cut p ()

ws :: Parse ()
ws = $(F.switch [| case _ of
  "\t" -> ws
  "\n" -> ws
  "\r" -> ws
  " " -> ws
  _ -> return () |])

name :: Parse ()
name = do
  F.skipSatisfyAscii F.isLatinLetter
  F.skipMany $ F.skipSatisfyAscii \c -> F.isLatinLetter c || F.isDigit c

withNameSpan :: (F.Span -> Parse a) -> Parse a
withNameSpan k = F.withSpan name \_ x -> ws *> k x

expr :: Parse Expr
expr = assert $(F.switch [| case _ of
  "\\" -> ws *> lam
  "λ" -> ws *> lam
  "(" -> ws *> expr <* assert $(F.char ')') <* ws >>= app
  _ -> withNameSpan (app . Var) |])

lam :: Parse Expr
lam = assert $ withNameSpan \x -> Lam x <$> $(F.switch [| case _ of
  "," -> ws *> expr
  _ -> lam |])

var :: Parse Expr
var = withNameSpan (return . Var)

app :: Expr -> Parse Expr
app f = F.branch $(F.char '(')
  (ws *> expr >>= \a -> assert $(F.char ')') *> app (App f a))
  (F.withOption var (\a -> app (App f a)) (return f))

parse :: ByteString -> Maybe Expr
parse str =
  case F.runParser (ws *> expr <* F.eof) str of
    F.OK e _ -> Just e
    _ -> Nothing
