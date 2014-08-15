#!/bin/bash

CABIN_HOME=$HOME/.cabin
CABIN_CABIN=$CABIN_HOME/cabins/cabin
CABIN_PATH=$CABIN_HOME/bin

cd $HOME
mkdir -p "$CABIN_HOME/{bin,cabins,loaded}"

# Install the 'cabin' cabin
cd $CABIN_HOME/cabins
git clone git@github.com:nc6/cabin.git && cd $CABIN_CABIN
cabal sandbox init
cabal install

# Load the cabin cabin
CABIN_BIN=$CABIN_CABIN/.cabal-sandbox/bin/cabin

$CABIN_BIN reindex #Build the index
$CABIN_BIN load cabin

export PATH=$PATH:$CABIN_PATH

echo "export PATH=\$PATH:$CABIN_PATH" >> $HOME/.profile