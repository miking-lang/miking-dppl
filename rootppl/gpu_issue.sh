#!/bin/bash
CPUCORES=32

echo "Compiling ClaDS classic with stack for the GPU"
make clean
make model=models/phylogenetics/clads2/TEMPLATE-clads2.cu arch=75 -j$CPUCORES > /dev/null 2> /dev/null
./program 10000 50


echo "Compiling ClaDS factorized with stack for the GPU"
make clean
make model=models/phylogenetics/clads2/TEMPLATE-clads2-factor.cu arch=75 -j$CPUCORES  > /dev/null 2> /dev/null
./program 10000 50


echo "Compiling ClaDS delayed with stack for the GPU"
make clean
make model=models/phylogenetics/clads2/TEMPLATE-clads2-delayed.cu arch=75 -j$CPUCORES > /dev/null 2> /dev/null 
./program 10000 50


echo "Compiling ClaDS delayed with stack for the GPU with side-branch simulation turned off"
make clean
make model=models/phylogenetics/clads2/TEMPLATE-clads2-delayed-nohidden.cu arch=75 -j$CPUCORES  > /dev/null 2> /dev/null
./program 10000 50
