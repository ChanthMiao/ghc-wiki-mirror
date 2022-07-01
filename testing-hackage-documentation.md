This short guide tells you how to set-up a local hackage instance to test uploading
documentation and packages to.

This is useful for people who want to

1. Verify that the hackage documentation is correct.
2. Test other features of GHC such as reinstallable GHC etc

# Setting up hackage-server

Clone hackage-server:

```
git clone git@github.com:haskell/hackage-server.git
```

Follow [the guide](https://github.com/haskell/hackage-server#installing-dependencies) to setting up the environment to build the server. I use
NixOS so I just write `nix-shell` to do this.

Initialise the server state:

```
cabal v2-run -- hackage-server init
```

Start the server

```
cabal v2-run -- hackage-server run --static-dir=datafiles/ --base-uri=http://127.0.0.1:8080
```

Add the admin user to the uploader group

1. Navigate to http://127.0.0.1:8080/packages/uploaders/edit
2. User: admin
3. Click "Add member"

Your hackage server is now ready to recieve packages.

# Building the sdists and documentation

Build a bindist with `--haddock-base-url=http://127.0.0.1:8080/package/%pkg%/docs`

```
./hadrian/build binary-dist-dir --haddock-base-url=http://127.0.0.1:8080/package/%pkg%/docs`
```

The resulting bindist dir will be in `_build/bindist/ghc-*`.

Now the documentation has been built. We use the `.gitlab/upload_ghc_libs.py` script to create
the source dists and documentation tarballs.

## Creating the documentation

Run

```
.gitlab/upload_ghc_libs.py prepare --bindist _build/bindist/ghc-*-x86_64-unknown-linux/
```

This will produce a folder `.upload-libs/` which contains all the source dists and
documentation tarballs for the packages we want to upload.

## Uploading the packages

Now we are going to upload the packagages to our local server.


### Local cabal.config

You need to create a local cabal.config file which specifies the address of your local
hackage server and the local credentials. This is just used for the upload step.

```
repository local_hackage
  url: http://localhost:8080

username: admin
password: admin
```

### Upload Command

The following command can be used to upload the packages.

```
CABAL_CONFIG=/path/to/local/config .gitlab/upload_ghc_libs.py upload --docs=.upload-libs/docs --publish
```

The packages can then be viewed on the server by navigating to `localhost:8080` in
your browser.


