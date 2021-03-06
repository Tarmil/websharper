#!/bin/bash

set -e

if [ "$OS" = "Windows_NT" ]; then
    fake() { packages/build/FAKE/tools/FAKE.exe "$@" --fsiargs build.fsx; }
else
    fake() { mono packages/build/FAKE/tools/FAKE.exe "$@" --fsiargs -d:MONO build.fsx; }
fi

paket restore --touch-affected-refs

if [ "$VisualStudioVersion" == ""  ]; then
    export VisualStudioVersion=15.0
fi

fake "$@"
