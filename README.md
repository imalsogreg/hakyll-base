# hakyll-base

A simple Hakyll template site using Yahoo's Pure CSS for navigation

Before running, to make github deployment work

```bash
git submodule add git@github.com:yourusername/yourusername.github.io _site
```

## Installation

```bash
cabal sandbox init
cabal install --only-dependencies
cabal install
.cabal-sandbox/bin/site preview
```

## Deployment
./deploy.sh
