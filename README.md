A haskell module for error handling.

Exposes the "Dangerous" Monad (and accompanying DangerousT transformer),
which encapsulates computations that can exit early and produce warnings.

The Dangerous monad does not preform any IO, so you can handle the errors
and warnings at any point in your code.

The module does, however, offer an 'execute' function which encapsulates
the "normal" use case of writing warnings to stderr and then exiting if
necessary.

Dangerous monads of computation type ``a`` result in ``(Either Exit a, [Warning])``
where Exit can denote either computation ending early (``Stop``) or computation
failing (``Exit`` with an error code, ``Failure`` without).
