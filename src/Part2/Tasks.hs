module Part2.Tasks where

import Util (notImplementedYet)

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term
  = IntConstant {intValue :: Int} -- числовая константа
  | Variable {varName :: String} -- переменная
  | BinaryTerm {op :: BinaryOp, lhv :: Term, rhv :: Term} -- бинарная операция
  deriving (Show, Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) = BinaryTerm Plus

infixl 6 |+|

(|-|) :: Term -> Term -> Term
(|-|) = BinaryTerm Minus

infixl 6 |-|

(|*|) :: Term -> Term -> Term
(|*|) = BinaryTerm Times

infixl 7 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement (Variable a) = if a == varName then replacement else Variable a
replaceVar varName replacement (BinaryTerm op a b) = BinaryTerm op (replaceVar varName replacement a) (replaceVar varName replacement b)
replaceVar varName replacement expression = expression

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate (BinaryTerm Plus (IntConstant a) (IntConstant b)) = IntConstant (a + b)
evaluate (BinaryTerm Minus (IntConstant a) (IntConstant b)) = IntConstant (a - b)
evaluate (BinaryTerm Times (IntConstant a) (IntConstant b)) = IntConstant (a * b)
evaluate (BinaryTerm op (BinaryTerm op1 a1 b1) (BinaryTerm op2 a2 b2)) =
  evaluate $ BinaryTerm op (evaluate $ BinaryTerm op1 a1 b1) (evaluate $ BinaryTerm op1 a2 b2)
evaluate exp = exp
