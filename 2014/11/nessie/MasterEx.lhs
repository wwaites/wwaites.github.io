\hide{
\begin{code}
module Main where

import Master
import Numeric.LinearAlgebra
import qualified Numeric.LinearAlgebra.Data as LD
\end{code}
}
\begin{figure}
\begin{center}
\includegraphics[width=0.45\textwidth]{dissipation_0001.eps}
\includegraphics[width=0.45\textwidth]{dissipation_0010.eps}
\includegraphics[width=0.45\textwidth]{dissipation_0020.eps}
\includegraphics[width=0.45\textwidth]{dissipation_0040.eps}
\end{center}
\caption{Probability distribution evolution for the dissipation
reaction. Top row is $t=0.1$ and $t=1$, bottom row is $t=2$ and
$t=4$. The initial distribution is 25\% probability for 10, 100,
200 or 300 particles, and 0 otherwise.}
\label{fig:dissipation}
\end{figure}

\subsection*{Example: Dissipation}
Here is a simple driver program to compute the dissipation reaction,
\begin{equation}
  A \rightarrow \emptyset
\end{equation}
where it is fed with an initial probability distribution of equal
probability to have 10, 100, 200 or 300 particles. This particular
example is chosen because it is the same one that was demonstrated
using the MTM and shows that, in this case, the same results can be
obtained directly. This should not be surprising, after all the
example is one of the simplest possible so no conclusions can be drawn
based on these results about how the MTM and the direct methods would
compare on more complicated problems.
\begin{code}
main :: IO ()
main =
  let size = 330
      h = hamiltonian size [(0, 1, 1)]
      x = trans $ LD.toDense [((0,10), 0.25)
                             , ((0,100), 0.25)
                             , ((0,200), 0.25)
                             , ((0,300), 0.25)
                             ,((0,size-1), 0)
                             ]
  in timeseries "dissipation" 100 0.1 h x
\end{code}
The results are plotted in figure~\ref{fig:dissipation}, and show the
familiar Loch Ness Monster curve. It takes about 15 seconds to run for
100 time steps on a reasonably modern laptop computer.

% Local Variables:
% compile-command: "ghc -O2 -Wall -Werror --make MasterEx.lhs"
% End:
