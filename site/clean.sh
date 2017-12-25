#!/bin/bash

for file in *; do
	if [ -d $file ]; then
	    pushd $file;
		rm *.elc;
		popd;
	fi
done
		
