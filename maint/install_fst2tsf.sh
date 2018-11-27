#!/bin/bash
Where=$(readlink -e ${0})
Where=${Where%/bin/*}
echo ===== installing into $Where =====
cd $Where/src
make clean
make install RMNLIB=$RMNLIB
make clean
