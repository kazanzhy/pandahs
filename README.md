# pandahs

## Setup

#### Prerequisites
* Stack (required): [installation](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
* GHC and Cabal (optional): [installation using GHCup](https://www.haskell.org/ghcup/)
* Intellij-Haskell plugin (optional)

#### Initial
```bash
stack new pandahs
cd pandahs
stack setup
stack build
stack exec pandahs-exe
```
```bash
git init
git remote add origin git@github.com:kazanzhy/pandahs.git
-- Edit .gitignore
-- Edit pandahs.cabal
-- Edit stack.yaml
-- Edit package.yaml
git add .
git commit -m "Init haskell project"
git push
```

#### Work
```bash
git remote add origin git@github.com:kazanzhy/pandahs.git
cd pandahs
stack setup
stack build
stack exec pandahs-exe
```
