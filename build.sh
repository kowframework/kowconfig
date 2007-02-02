#!/bin/sh

# This script calls gnatmake using the adaconfig.gpr project file.


ADA_PROJECT_PATH=.
for i in ../*;
do
	ADA_PROJECT_PATH=${ADA_PROJECT_PATH}:$i;
done


echo $ADA_PROJECT_PATH
export ADA_PROJECT_PATH

gnatmake -P ada_config.gpr
