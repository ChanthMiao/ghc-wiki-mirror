## Haskell

If all dependencies to build GHC (with Hadrian) are installed, the [Haskell plugin](https://marketplace.visualstudio.com/items?itemName=haskell.haskell) works out of the box. It installs the required [Haskell Language Server](https://github.com/haskell/haskell-language-server) automatically.

## C

There is a [`compile_flags.txt`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/compile_flags.txt) that should automatically be picked up by the [clangd plugin](https://marketplace.visualstudio.com/items?itemName=llvm-vs-code-extensions.vscode-clangd).

As it depends on `-I.hie-bios/stage0/lib` being initialized, it only works in conjunction with the [Haskell plugin](#Haskell).

## Nix

If you're using [ghc.nix](https://github.com/alpmestan/ghc.nix), you'll very likely want use it the provide the dependencies for the previously described plugins.

This can easily be done with the [Nix Environment Selector](https://marketplace.visualstudio.com/items?itemName=arrterian.nix-env-selector) plugin.

## Git Submodules
By default VSCode only detects 10 Git submodules. That's less than the number of submodules the GHC project uses. To fix this increase the setting `git.detectSubmodulesLimit`.