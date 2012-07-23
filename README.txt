#########################################################################
#			AdaWorks :: Config				#
#########################################################################
#									#
#	This is a library for handling config files.			#
#									#
#									#
#	It's released by the GNAT Modified License. For more information#
#  refer to the files:							#
#		. LICENSE.txt						#
#		. GPL.txt						#
#									#
#	For installation instructions, pleas read the file manual.pdf	#
#########################################################################

This is the new version of the KOW_Config module.

Now there is support for only .cfg files - no more XML files. In the other
hand we will implement support for:

variables inside the configuration files, such as


some_key="value"
some_other_key="${some_key} is the value of the some_key key"

and so on




Installation:

The usual ./configure && make all install

It'll install in the GNAT path.
