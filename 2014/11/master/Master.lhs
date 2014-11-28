\hide{
\begin{code}
module Master where

import Numeric.LinearAlgebra
import qualified Numeric.LinearAlgebra.Data as LD
import Text.Printf
infix 5 !.
\end{code}
}

This implementation of the Master Equation is a bit naive and is based
on the observation that the creation and annihilation operators,
$a^{\dagger}$ and $a$ are really matrices.
We start with the Master Equation,
\begin{equation}
\label{eq:master}
\frac{d\left|\psi(t)\right>}{dt} = H\left|\psi(t)\right>
\end{equation}
and the reaction Hamiltonian written in t.erms of creation, $a^\dagger$
and annihilation $a$ operators as,
\begin{equation}
H = \sum_{\tau \in T} r_\tau(a^{\dagger t_\tau} - a^{\dagger s_\tau})a^{s_\tau}
\end{equation}
where $T$ is our set of reactions.

The solution of equation \ref{eq:master} is,
\begin{equation}
\label{eq:evolution}
\left|\psi(t)\right> = e^{tH}\left|\psi(0)\right>
\end{equation}
and we want to have a way of evaluating that exponential.

So start by defining our operators. Spelling it out, the creation
operator, $a^\dagger$ looks like this,
\begin{equation}
a^\dagger = \left [
  \begin{array}{cccccc}
    0 & 0 & 0 & 0 & 0 & \cdots\\
    1 & 0 & 0 & 0 & 0 & \\
    0 & 1 & 0 & 0 & 0 & \cdots\\
    0 & 0 & 1 & 0 & 0 & \\
    0 & 0 & 0 & 1 & 0 & \ddots\\
    \vdots &  & \vdots & & \ddots & \ddots
  \end{array}
  \right]
\end{equation}
This can be generated easily from a sparse association list, for
arbitrary size. It is transformed to a dense matrix simply because the
matrix exponential function \emph{expm} that we will eventually use
expects a dense matrix. It is easy to imagine one that operates on
sparse matrices, does things in parallel, uses the GPU, and so forth
but premature optimisation is the enemy of getting things done.
\begin{code}
create :: Int -> Int -> Matrix Double
create n p = LD.toDense $ ((n-1, n-1), 0):m
  where m = [ ((j+p, j), 1) | j <- [0..(n-p-1)]]
\end{code}
The first argument to \lstinline{create} is the size of the matrix to
produce, and the second one, as an optimisation, is the power. So this
function produces a matrix of size $n$ for $a^{\dagger p}$.

Whereas the action of the creation operator is,
\begin{equation}
a^\dagger\ket{n} = \ket{n+1}
\end{equation}
the annihilation operator $a$ is very slightly more complicated,
\begin{equation}
a\ket{n} = \begin{cases}
  n\ket{n-1}&n > 0\\
  0&\text{otherwise}
\end{cases}
\end{equation}
This can be explicitly represented as an infinite matrix in a similar
way,
\begin{equation}
a = \left [
  \begin{array}{cccccc}
    0 & 1 & 0 & 0 & 0 & \cdots\\
    0 & 0 & 2 & 0 & 0 & \\
    0 & 0 & 0 & 3 & 0 & \cdots\\
    0 & 0 & 0 & 0 & 4 & \\
    0 & 0 & 0 & 0 & 0 & \ddots\\
    \vdots &  & \vdots & & \ddots & \ddots
  \end{array}
  \right]
\end{equation}
To implement this in code we need either to make a bunch of matrix
multiplications, or to implement a falling factorial and construct a
sparse matrix as we did above. We do the latter.
\begin{code}
(!.) :: Int -> Int -> Int
n !. k = product [(n-k+1)..n]
\end{code}
and then $a^p$ is implemented in code,
\begin{code}
annihilate :: Int -> Int -> Matrix Double
annihilate n p = LD.toDense $ ((n-1, n-1), 0):m
  where m = [ ((j, j+p), fromIntegral (j+p !. p)) | j <- [0..(n-p-1)] ]
\end{code}

With these in hand, we can define reactions as they appear in the
Hamiltonian above, with a certain matrix size as before, $n$, and a
certain number of instances of particles that they create $t$ or
destroy $s$ at a certain rate, $r$,
\begin{code}
reaction :: Int -> Int -> Int -> Double -> Matrix Double
reaction n t s r = scale r (ct <> as - cs <> as)
  where
    ct = create n t
    cs = create n s
    as = annihilate n s
\end{code}

For convenience, we can make a function to create for us Hamiltonians,
of size $n$ for a list of reaction specifications,
\begin{code}
hamiltonian :: Int -> [(Int, Int, Double)] -> Matrix Double
hamiltonian n rs = foldl1(+) $ map(\(t, s, r) -> reaction n t s r) rs
\end{code}
and an evolution function that, given a time, initial state and a
hamiltonian, returns a new state,
\begin{code}
evolve :: Double -> Matrix Double -> Matrix Double -> Matrix Double
evolve t h x = (expm $ scale t h) <> x
\end{code}
This last function implements~\ref{eq:evolution}, the solution to the
Master Equation.

Finally a utility function that conducts the evolution and saves each step to 
disk,
\begin{code}
timeseries :: FilePath -> Int -> Double ->
              Matrix Double -> Matrix Double -> IO ()
timeseries base n dt h x = mapM_ (\k -> dump k (fromIntegral k*dt)) [1..n]
  where
    filename k = printf "%s_%04d.dat" base k
    dump k t   = 
      saveMatrix (filename k) "%f" $ evolve t h x
\end{code}
Again, much room for optimisation here, especially since each step can
be carried out in parallel.
