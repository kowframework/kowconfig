#!/usr/bin/env bash
# Installer for KOW Framework applications
#
# @author Marcelo C. de Freitas
#
# All this script does is to recursivelly install everything from $work_path inside $prefix




source scripts/buildutil.sh


load_configuration


iterate_filelist install_item
