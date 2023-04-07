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
withNameSpan k = assert $ F.withSpan name \_ x -> ws *> k x

expr :: Parse Expr
expr = assert $(F.switch [| case _ of
  "\\" -> ws *> lam
  "λ" -> ws *> lam
  _ -> app |])

lam :: Parse Expr
lam = withNameSpan \x -> Lam x <$> $(F.switch [| case _ of
  "," -> ws *> expr
  _ -> lam |])

app :: Parse Expr
app = simple >>= go
  where
  go :: Expr -> Parse Expr
  go f = F.branch $(F.char '(')
    (ws *> expr >>= \a -> assert $(F.char ')') *> go (App f a))
    (F.withOption var (\a -> go (App f a)) (return f))

  -- Don't use `withNameSpan` because `var` should fail if `name` fails and not
  -- generate a parse error.
  var :: Parse Expr
  var = F.withSpan name \_ x -> ws *> return (Var x)

  simple :: Parse Expr
  simple = F.branch $(F.char '(')
    (ws *> expr <* assert $(F.char ')') <* ws)
    (withNameSpan (return . Var))

parse :: ByteString -> Maybe Expr
parse str =
  case F.runParser (ws *> expr <* F.eof) str of
    F.OK e _ -> Just e
    _ -> Nothing
