#!/bin/sh

# This script calls gnatmake using the adaconfig.gpr project file.



# Add other AdaWorks projects to the path:
ADA_PROJECT_PATH=.
for i in ../*;
do
	ADA_PROJECT_PATH=${ADA_PROJECT_PATH}:$i;
done

# Add XML/Ada to the path:
ADA_PROJECT_PATH="${ADA_PROJECT_PATH}:`xmlada-config --libs | cut -d "L" -f2 | cut -d " " -f1`/gnat"

echo $ADA_PROJECT_PATH



echo $ADA_PROJECT_PATH
export ADA_PROJECT_PATH

gnatmake -P ada_config.gpr
