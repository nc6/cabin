Introduction
====

Cabin (CAbal BINary sandbox) is a tool to manage cabal binary sandboxes. It
lets you install a binary program from cabal into an individual sandbox, and
then load that sandbox's bin directory into your home environment. For example:

```
cabin install alex
cabin load alex
```

Would install `alex` into its own sandbox and link the relevant binaries into
your $PATH.

Installation
====

There are two easy ways to install:

From Hackage
-----

    cabal --no-require-sandbox install cabin

will install cabin in your default cabal profile, whence you can run it:

    $HOME/.cabal/bin/cabin

You also need to add the cabin binary path to your PATH:

    export PATH=$PATH:$HOME/.cabin/bin

And add this line to the relevant `.profile` file.

From github
------

    wget -O - https://github.com/nc6/cabin/blob/master/bootstrap.sh | bash

Should bootstrap the operation and install `cabin` into its own cabin, which
will be loaded for you. Note that if you then call `cabin unload cabin`, it will
no longer be available and you will have to invoke the `cabin` binary directly
from within `$HOME/.cabin/cabins/cabin/.cabal-sandbox/bin/cabin`.
