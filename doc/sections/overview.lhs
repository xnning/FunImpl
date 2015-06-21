%include lhs2TeX.fmt

\section{Overview}

\bruno{Jeremy: can you give this section a go and start writing it up? I think this section should be your priority for now.}

We begin this section with an informal introduction to the main features of \name. We show how it can serve as a simple and compiler-friendly core language with general recursion and decidable type system. The formal details are presented in \S\ref{sec:formal}.

\subsection{Calculus of Constructions}
\label{sec:coc}

\name is based on the \emph{Calculus of Constructions} (\coc)~\cite{coc}, which is a higher-order typed lambda calculus. One ``unconventional'' feature of \coc is the so-called \emph{conversion} rule as shown below:
\ottusedrule{\ottdruleTccXXConv{}}

The conversion rule allows one to derive $e:[[t2]]$ from the derivation of $e:[[t1]]$ and the $\beta$-equality of $[[t1]]$ and $[[t2]]$. Note that in \coc, the use of this rule is implicit in that it is automatically applied during type checking to all non-normal form terms. To illustrate, let us consider a simple example. Suppose we have a built-in base type $[[int]]$ and \[f \equiv [[\x:(\y:star.y)int.x]] \] Without the conversion rule, $f$ cannot be applied to, say $3$ in \coc. Given that $f$ is actually $\beta$-convertible to $[[\x:int.x]]$, the conversion rule would allow the application of $f$ to $3$ by implicitly converting $[[\x:(\y:star.y)int.x]]$ to $[[\x:int.x]]$.

\subsection{Explicit Type Conversion Rules}

\bruno{Contrast our calculus with the calculus of constructions. Explain fold/unfold.}

In contrast to the implicit reduction rules of \coc, \name makes it explicit as to when and where to convert one type to another. To achieve that, it makes type conversion explicit by introducing two operations: $[[castup]]$ and $[[castdown]]$.

In order to have a better intuition, let us consider the same example from \S\ref{sec:coc}. In \name, $f\,3$ is intended as an ill-typed application. Instead one would like to write the application as \[ f\,([[castup[(\y:star.y)int]three]]) \] The intuition is that, $[[castup]]$ is actually doing type conversion since the type of $ 3 $ is $ [[int]] $ and $ [[(\y:star.y)int]] $ can be reduced to $ [[int]] $.

The dual operation of $[[castup]]$ is $[[castdown]]$. The use of $[[castdown]]$ is better explained by another similar example. Suppose that \[ g \equiv [[\x:int.x]] \] and term $z$ has type \[ [[(\y:star.y)int]] \] $ g\,z $ is again an ill-typed application, while $ g\,([[castdown z]]) $ is type correct because $[[castdown]]$ reduces the type of $ z $ to $ [[int]] $.

\subsection{Decidability and Strong Normalization}

\bruno{Informally explain that with explicit fold/unfold rules the decidability of the
type system does not depend on strong normalization.}

The decidability of the type system of \coc depends on the normalization property for all constructed terms~\cite{coc:decidability}. However strong normalization does not hold with general recursion. This is simply because due to the conversion rule, any non-terminating term would force the type checker to go into an infinitely loop (by constantly applying the conversion rule without termination), thus rendering the type system undecidable.

With explicit type conversion rules, however, the decidability of the type system no longer depends on the normalization property. In fact \name is not strong normalizing, as we will see in later sections. The ability to write non-terminating terms motivates us to have more control over type-level computation. To illustrate, let us consider a contrived example. Suppose that $d$ is a ``dependent type'' where \[d : [[int -> star]]\] so that $d\,3$ or $d\,100$ all yield the same type.  With general recursion at hand, we can image  a term $z$ that has type \[d\,\mathsf{loop}\] where $\mathsf{loop}$ stands for any diverging computation and of type $[[int]]$. What would happen if we try to type check the following application: \[ [[(\x: d three.x)z]]\] Under the normal typing rules of \coc, the type checker would get stuck as it tries to do $\beta$-equality on two terms: $d\,3$ and $d\,\mathsf{loop}$, where the latter is non-terminating.

This is not the case for \name: (i) it has no such conversion rule, therefore the type checker would do syntactic comparison between the two terms instead of $\beta$-equality in the above example; and (ii) one would need to write infinite number of $[[castdown]]$'s to make the type checker loop forever (e.g., $([[\x:d three.x]])([[castdown]]([[castdown]] \dots z) $), which is impossible in reality.

In summary, \name achieves the decidability of type checking by explicitly controlling type-level computation, which is independent of the normalization property, while supporting general recursion at the same time.

\subsection{Unifying Recursive Types and Recursion}

\bruno{Show how in \name recursion and recursive types are unified.
Discuss that due to this unification the sensible choice for the
evaluation strategy is call-by-name. }

Recursive types arise naturally if we want to do general recursion. \name differs from other programming languages in that it unifies both recursion and recursive types by the same $\mu$ primitive.

\subsubsection{Recursive types}

In the literature on type systems, there are two approaches to recursive types. One is called \emph{equi-recursive}, the other \emph{iso-recursive}. \name takes the latter approach since it is more intuitive to us with regard to recursion. The \emph{iso-recusive} approach treats a recursive type and its unfolding as different, but isomorphic. In \name, this is witnessed by first $[[castup]]$, then $[[castdown]]$. A classic example of recursive types is the so-called ``hungry'' type: $H = \miu{\sigma}{\star}{\mathsf{Int} \rightarrow \sigma}$. A term $z$ of type $H$ can accept any number of numeric arguments and return a new function that is hungry for more, as illustrated below:
\begin{align*}
[[castdown z]] &: [[int]][[->]]H  \\
[[castdown(castdown z)]] &: [[int]][[->]][[int]][[->]]H \\
[[castdown]]([[castdown]] \dots z) &: [[int]][[->]][[int]][[->]]\dots[[->]]H
\end{align*}

\subsubsection{Recursion}

The same $\mu$ primitive can also be used to define recursive functions, e.g., the factorial function: \[\miu{f}{\mathsf{Int} \rightarrow \mathsf{Int}}{\lam{x}{\mathsf{Int}}{\mathsf{if}\,(x == 0)\,\mathsf{then}\,1\,\mathsf{else}\,x * f\,(x - 1)}}\] This is reflected by the dynamic semantics of the $\mu$ primitive:
\[\miu{x}{T}{E} \longrightarrow E[x:=\miu{x}{T}{E}]\]
which is exactly doing recursive unfolding of the same term.

Due to the unification, the \emph{call-by-value} evaluation strategy does not fit in our setting. In call-by-value evaluation, recursion can be expressed by the recursive binder $\mu$ as $\mu f : T \rightarrow T.\, E$ (note that the type of $f$ is restricted to function types). Since we don't want to pose restrictions on the types, the \emph{call-by-name}  evaluation is a sensible choice.

% The dynamic semantics of $\mu$ requires the recursive binder to satisfy (omit type annotations for clarity):  \[ \mu f.\,E = (\lambda f.\,E) (\mu f.\,E) \] which, however, does not terminate in strict languages. Therefore, to loosen the function-type restriction to allow any types, the sensible choice for the evaluation strategy is \emph{call-by-name}.

\subsection{Encoding Datatypes}

\bruno{Informally explain how to encode recursive datatypes and recursive functions
using datatypes.}

%format . = ".\,"
%format mu = "\mu"
%format pi = "\Pi"
%format * = "\star"
%format castup = "\mathsf{cast}^\uparrow"
%format castdown = "\mathsf{cast}_\downarrow"

With the explicit type conversion rules and the $\mu$ primitive, it is straightforward to encode recursive datatypes and recusive functions using datatypes. While inductive datatypes can be encoded using either the Church or the Scott encoding, we adopt the Scott encoding as it is bear some resemblance to case analysis, making it more convenient to encode pattern matching. We demonstrate the encoding method using a simple datatype as a running example: the natural numbers.

The datatype declaration for natural numbers is:
\begin{figure}[h!]
\begin{spec}
  data Nat = Z | S Nat;
\end{spec}
\end{figure}

In the Scoot encoding, the encoding of the \emph{Nat} type reflects how its two constructors are going to be used. Since \emph{Nat} is a recursive datatype, we have to use recursive types at some point to reflect its recursive nature. As it turns out, the \emph{Nat} type can be simply represented as |mu X : * . pi B : * . B -> (X -> B) -> B|.

As can be seen, in the function type |B -> (X -> B) -> B|, $B$ corresponds to the type of the \emph{Z} constructor, and |X -> B| corresponds to the type of the \emph{S} constructor. The intuition is that any use of the datatype being defined in the constructors is replaced with the recursive type, except for the return type, which is a type variable for use in the recursive functions.

Now its two constructors can be encoded correspondingly as below:
\begin{figure}[h!]
\begin{spec}
  let Z : Nat = castup[Nat] (\ B : * . \ z : B . \ f : Nat -> B . z)
  in
  let S : Nat -> Nat = \ n : Nat .
    castup[Nat] (\ B : * . \ z : B . \ f : Nat -> B . f n)
  in
\end{spec}
\end{figure}

Thanks to the explicit type conversion rules, we can make use of the $[[castup]]$ operation to do type conversion between the recursive type and its unfolding.

As the last example, let us see how we can define recursive functions using the \emph{Nat} datatype. A simple example would be recursively adding two natural numbers, which can be defined as below:

\begin{figure}[h!]
\begin{spec}
  let add : Nat -> Nat -> Nat = mu f : Nat -> Nat -> Nat .
    \ n : Nat . \ m : Nat .
      (castdown n) Nat m (\ n' : Nat . S (f n' m))
\end{spec}
\end{figure}

As we can see, the above definition quite resembles case analysis common in modern functional programming languages. (Actually we formalize the encoding of case analysis in \S\ref{sec:surface}.)

Due to the unification of recursive types and recursion, we can use the same $\mu$ primitive to write both recursive types and recursion with ease.

%%% Local Variables:
%%% mode: latex
%%% TeX-master: "../main"
%%% End:
