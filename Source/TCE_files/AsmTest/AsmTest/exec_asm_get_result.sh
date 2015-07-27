#!/bin/bash

adf=$1
asm=$2
unit_size=$3
num_of_units=$4
address=$5

script_dir=$(cd $(dirname $0) && pwd);
cd $script_dir

tceasm -o temp.tpef $adf $asm

ttasim -a $adf -p temp.tpef << SIM

x /f output /u $unit_size /n $num_of_units $address
exit

SIM

rm -f temp.tpef


