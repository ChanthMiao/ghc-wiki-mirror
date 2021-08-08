Following the approval of #19884, tags generation inside of GHCi will be deprecated. 

A companion tool called [ghc-tags][ghc-tags] has been chosen as replacement.

If you only use one of the two flavors of tags, you can simply name the macro command 'tags'.
```
echo >> ~/.ghci ':def! ctags \x -> pure $ ":!ghc-tags --ctags -o \"" ++ x ++ "\" ."'
echo >> ~/.ghci ':def! etags \x -> pure $ ":!ghc-tags --etags -o \"" ++ x ++ "\" ."'
```
If you don't specify any path, the default output file will be `tags` (in the case of ctags) and `TAGS` (in the case of etags). The `*.mtime` file will also be generated alongside the tag files.

[ghc-tags]: https://hackage.haskell.org/package/ghc-tags