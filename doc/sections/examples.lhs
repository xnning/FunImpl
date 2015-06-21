%include lhs2TeX.fmt

%format . = ".\,"
%format mu = "\mu"
%format * = "\star"
%format letrec = "\mathbf{letrec}"


\section{Applications}
\label{sec:app}

In this section, we show applications, which either Haskell needs non-trivial extensions to do that, or languages like Coq and Agda are impossible to do, whereas we can easily achieve in \name.

% \subsection{Parametric HOAS}
% \label{sec:phoas}

% Parametric Higher Order Abstract Syntax (PHOAS) is a higher order approach to represent binders, in which the function space of the meta-language is used to encode the binders of the object language. We show that \name can handle PHOAS by encoding lambda calculus as below:

% \begin{figure}[h!]
% \begin{spec}
% data PLambda (a : *) = Var a
%    | Num nat
%    | Lam (a -> PLambda a)
%    | App (PLambda a) (PLambda a);
% \end{spec}
% \end{figure}

% Next we define the evaluator for our lambda calculus. One advantage of PHOAS is that, environments are implicitly handled by the meta-language, thus the type of the evaluator is simply |plambda value -> value|. The code is presented in Figure~\ref{fig:phoas}.

% \begin{figure}[ht]
% \begin{spec}
% data Value  = VI nat
%    | VF (Value -> Value);
% let eval : PLambda Value -> Value =
%    mu ev : PLambda Value -> Value .
%      \ e : PLambda Value . case e of
%        Var (v : Value) => v
%      | Num (n : nat)   => VI n
%      | Lam (f : Value -> PLambda Value) =>
%          VF (\ x : Value . ev (f x))
%      | App (a : PLambda Value) (b : PLambda Value) =>
%         case (ev a) of
%           VI (n : nat)            => VI n -- impossible to reach
%         | VF (f : Value -> Value) => f (ev b)
% in
% \end{spec}
%   \caption{Lambda Calculus in PHAOS}
%   \label{fig:phoas}
% \end{figure}

% Now we can evaluate some lambda expression and get the result back as in Figure~\ref{fig:pex}

% \begin{figure}[ht]
% \begin{spec}
% let show : Value -> nat =
%   \ e : Value . case e of
%     VI (n : nat)            => n
%   | VF (f : Value -> Value) => 10000 -- impossible to reach
% in
% let example : PLambda Value =
%   App Value
%       (Lam Value (\ x : Value . Var Value X))
%       (Num Value 42)
% in show (eval example) -- return 42
% \end{spec}
% \caption{Example of using PHOAS}
% \label{fig:pex}
% \end{figure}

\subsubsection{Conventional datatypes}

Conventional datatypes like natural numbers or polymorphic lists can be easily defined in \name, as in Haskell. For example, below is the definition of polymorphic lists:

\begin{figure}[H]
\begin{code}
  data List (a : *) = Nil | Cons a (List a);
\end{code}
\end{figure}

Because \name is explicitly typed, each type parameter needs to be accompanied with corresponding kind expressions. The use of the above datatype is best illustrated by the \emph{length} function:

\begin{figure}[h!]
  \begin{code}
    letrec length : (a : *) -> List a -> nat =
      \ a : * . \l : List a . case l of
        Nil => 0
      | Cons (x : a) (xs : List a) =>
        1 + length a xs
    in
    let test : List nat = Cons nat 1 (Cons nat 2 (Nil nat))
    in length nat test -- return 2
  \end{code}
\end{figure}

\subsubsection{HOAS}

\emph{Higher-order abstract syntax} is a generalization of representing programs where the function space of the meta-language is used to encode the binders of the object language. Because the recursive mention of the datatype can appear in a negative position, systems like Coq and Agda would reject programs using HOAS due to the restrictiveness of their termination checkers. However \name is able to express HOAS in a straightforward way. We show an example of encoding simply typed lambda calculus:

\begin{figure}[h!]
\begin{code}
  data Exp = Num nat
    | Lam (Exp -> Exp)
    | App Exp Exp;
\end{code}
\end{figure}

Next we define the evaluator for our lambda calculus. As noted by [], the evaluation function needs an extra function \emph{reify} to invert the result of evaluation. The code is presented in Figure~\ref{fig:hoas}.

\begin{figure}[ht]
\begin{code}
data Value = VI nat | VF (Value -> Value);
rec Eval = Ev { eval' : Exp -> Value, reify' : Value -> Exp };
let f : Eval = mu f' : Eval .
  Ev (\ e : Exp . case e of
        Num (n : nat) => VI n
      | Lam (fun : Exp -> Exp) =>
          VF (\e' : Value . eval' f' (fun (reify' f' e')))
      | App (a : Exp) (b : Exp) =>
          case eval' f' a of
            VI (n : nat) => VI n -- abnormal branch
          | VF (fun : Value -> Value) => fun (eval' f' b))
     (\v : Value . case v of
       VI (n : nat) => Num n -- abnormal branch
     | VF (fun : Value -> Value) =>
         Lam (\e' : Exp . (reify' f' (fun (eval' f' e')))))
in let eval : Exp -> Value = eval' f in
\end{code}
  \caption{An evaluator for the HOAS-encoded lambda calculus.}
  \label{fig:hoas}
\end{figure}

The definition of the evaluator is quite straightforward, although it is worth noting that, because \name has yet have exception mechanism, we have to pattern match on all possibilities. (That is why we have \emph{abnormal} branches in the above code.) Thanks to the flexibility of the $\mu$ primitive, mutual recursion can be encoded by using records!

Evaluation of a lambda expression proceeds as follows:

\begin{figure}[h!]
  \begin{code}
  let test : Exp = App (Lam (\ x : Exp . x)) (Num 42)
  in show (eval test) -- return 42
  \end{code}
\end{figure}

\subsubsection{Nested datatypes}
\label{sec:binTree}

A perfect binary tree is a binary tree whose size is exactly a power of two. In Haskell, perfect binary trees are usually represented using nested datatypes. We show that \name is able to encode nested datatypes.

First we define a pair datatype as follows:

\begin{figure}[H]
\begin{spec}
  data PairT (a : *) (b : *) = P a b;
\end{spec}
\end{figure}

Using pairs, perfect binary trees are easily defined as below:

\begin{figure}[h!]
\begin{spec}
  data B (a : *) = One a | Two (B (PairT a a));
\end{spec}
\end{figure}

Notice that the recursive use of \emph{B} does not hold \emph{a}, but \emph{PairT a a}. This means every time we use a \emph{Two} constructor, the size of the pairs doubles. In case you are curious about the encoding of \emph{B}, here is the one:

\begin{figure}[h!]
\begin{spec}
  let B : * -> * = mu X : * -> * .
      \ a : * . (B : *) -> (a -> B) -> (X (PairT a a) -> B) -> B
  in
\end{spec}
\end{figure}

Because of the polymorphic recursive type ($\mu X : \star \rightarrow \star $) being used, it is fairly straightforward to encode nested datatypes.

To easily construct a perfect binary tree from a list, we define a help function that transform a list to a perfect binary tree as shown in Figure~\ref{fig:perfectB}.

\begin{figure}[ht]
\begin{spec}
  let pairs : (a : *) -> List a -> List (PairT a a) =
    mu pairs' : (a : *) -> List a -> List (PairT a a) .
      \ a : * . \ xs : List a .
        case xs of
          Nil => Nil (PairT a a)
        | Cons (y : a) (ys : List a) =>
            case ys of Nil =>
              Nil (PairT a a)
            | Cons (y' : a) (ys' : List a) =>
                Cons (PairT a a) (P a a y y') (pairs' a ys')
  in
  let fromList : (a : *) -> List a -> B a =
    mu from' : (a : *) -> List a -> B a .
      \ a : * . \xs : List a .
        case xs of
          Nil => Two a (from' (PairT a a) (pairs a (Nil a)))
        | Cons (x : a) (xs' : List a) =>
          case xs' of
            Nil => One a x
          | Cons (y : a) (zs : List a) =>
              Two a (from' (PairT a a) (pairs a xs))
  in
\end{spec}
  \caption{Construct a perfect binary tree from a list}
  \label{fig:perfectB}
\end{figure}

Now we can define an interesting function \emph{powerTwo}. Given a natural number $n$, it computes the largest natural number $m$, such that $2^{m} < n$:

\begin{figure}[H]
\begin{spec}
  let twos : (a : *) -> B a -> nat =
    mu twos' : (a : *) -> B a -> nat .
      \ a : * . \x : B a .
        case x of
          One (y : a) => 0
        | Two (c : B (PairT a a)) =>
            1 + twos' (PairT a a) c
  in
  let powerTwo : Nat -> nat =
    \ n : Nat . twos nat (fromList nat (take n (repeat 1)))
  in powerTwo (S (S (S (S Z)))) -- return 2
\end{spec}
\end{figure}
