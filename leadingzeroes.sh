#! /bin/bash
# This script applies leadingzeroes.awk script on each file in a directory
cd ~/oanda/1/
for a in *; do
	if [ -f "$a" ]; then # was it a file?
		cat $a | awk -f ~/Documents/leadingzeroes.awk > ~/oanda/1/clean/$a
	fi
done

