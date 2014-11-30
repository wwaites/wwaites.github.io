\hide{
\begin{code}
module Main where

import Master
import Text.Printf
import Numeric.LinearAlgebra
import qualified Numeric.LinearAlgebra.Data as LD
\end{code}
}
\subsection*{Example: $\emptyset \rightarrow A,\, 2A \rightarrow \emptyset$}
To try to get a handle on the necessary matrix sizes and the effect
of truncation, consider a slightly more complicated system,
\begin{equation}
  \begin{array}{ccc}
  \emptyset &\rightarrow& A\\
  2A &\rightarrow& \emptyset
  \end{array}
\end{equation}
where it is fed with the same initial probability distribution as
before. It would be expected that the probability distribution will
tend to a Dirac delta function at zero, and it actually does so
much faster than the previous simple dissipation reaction.
\begin{code}
main :: IO ()
main = 
  let size = 600
      h = hamiltonian size [(1, 0, 1), (0, 2, 1)]
      x = trans $ LD.toDense [((0,10), 0.25)
                             , ((0,100), 0.25)
                             , ((0,200), 0.25)
                             , ((0,300), 0.25)
                             ,((0,size-1), 0)
                             ]
      file = printf "dissipation_%04d" size
  in timeseries file 100 0.01 h x
\end{code}

The results for various matrix sizes are plotted in
figure~\ref{fig:dissipation2}. Not unexpectedly, the results for the
matrix that is just large enough to accomodate the initial conditions
is inaccurate, but for all sizes significantly larger the results are
identical, as can be verified with a simple command to strip out any
entries in the difference which are simply zero,
\begin{figure}[h]
\begin{center}
\includegraphics[width=0.5\textwidth]{dissipation2_0001.eps}
\end{center}
\caption{Probability distribution evolution for the $\emptyset
\rightarrow A,\, 2A \rightarrow \emptyset$ reaction system for various
matrix sizes. The purple line is for a matrix of size 301, the filled
green circles are for 400, the red circles are 500 and the blue cross
(and line) are 600.  The initial distribution is 25\% probability for
10, 100, 200 or 300 particles, and 0 otherwise.}
\label{fig:dissipation2}
\end{figure}
\begin{verbatim}
  $ diff -u dissipation_0600_0001.dat dissipation_0400_0001.dat |\
        sed '/[ \-]0.000000/d'
  --- dissipation_0600_0001.dat   2014-11-30 10:03:42.221620645 +0000
  +++ dissipation_0400_0001.dat   2014-11-30 09:51:20.737631392 +0000
  @@ -398,203 +398,3 @@
  $
\end{verbatim}
This suggests diminishing returns with matrix size, at least for
dissipative systems.

% Local Variables:
% compile-command: "ghc -O2 -Wall -Werror --make Dissipation2.lhs"
% End:
