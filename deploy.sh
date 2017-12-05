#!/bin/bash
set -o errexit -o nounset
PKG_REPO=$PWD
cd ..

CURRENT_VERSION=$(grep 'Version: .*' DESCRIPTION | awk '{print $2}')
CURRENT_PACKAGE=$(grep 'Package: .*' DESCRIPTION | awk '{print $2}')
export PKG_TARBALL=$CURRENT_PACKAGE'_'$CURRENT_VERSION'.tar.gz'

addToDrat(){
  mkdir drat; cd drat

  ## Set up Repo parameters
  git init
  git config user.name "stijnvanhoey"
  git config user.email "stijn.vanhoey@gmail.com"
  git config --global push.default simple

  ## Get drat repo
  git remote add upstream "https://$GH_TOKEN@github.com/inbo/drat.git"
  git fetch upstream 2>err.txt
  git checkout gh-pages

  Rscript -e "drat::insertPackage('$PKG_REPO/$PKG_TARBALL', \
    repodir = '.', \
    commit='Travis update $PKG_REPO: build $TRAVIS_BUILD_NUMBER')"
  git push 2>err.txt

}

addToDrat
