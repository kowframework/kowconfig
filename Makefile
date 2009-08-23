# Makefile for the AW_Config
#
# @author Marcelo Coraça de Freitas <marcelo@kow.com.br>

ifndef ($(PREFIX))
	PREFIX=/usr/local
endif

ifndef ($(INCLUDE_PREFIX))
	INCLUDE_PREFIX=$(PREFIX)/include/kowconfig
endif

ifndef ($(LIB_PREFIX))
	LIB_PREFIX=$(PREFIX)/lib
endif
ifndef ($(GPR_PREFIX)) 
	GPR_PREFIX=$(LIB_PREFIX)/gnat 
endif


################
# Main targets #
################
all: libs

libs:
	gnatmake -P "./kowconfig.gpr"

clean: 
	gnatclean -P kowconfig.gpr
	@echo "All clean"

docs:
	@-./gendoc.sh
	@echo "The documentation is generated by a bash script. Then it might fail in other platforms than Linux"

gprfile:
	@echo "Preparing GPR file.."
	@echo version:=\"$(VERSION)\" > gpr/kowconfig.def
	@echo prefix:=\"$(PREFIX)\" >> gpr/kowconfig.def
	@echo lib_prefix:=\"$(LIB_PREFIX)\" >> gpr/apq.def
	@echo include_prefix:=\"$(INCLUDE_PREFIX)\" >> gpr/apq.def
	@gnatprep gpr/kowconfig.gpr.in gpr/kowconfig.gpr gpr/kowconfig.def

gprclean:
	@rm -f gpr/*gpr
	@rm -f gpr/*.def


install: gprfile
	@echo "Installing files"
	install -d $(INCLUDE_PREFIX)
	install -d $(LIB_PREFIX)
	install -d $(GPR_PREFIX)
	install src/* -t $(INCLUDE_PREFIX)
	install lib/* -t $(LIB_PREFIX)
	install gpr/*.gpr -t $(GPR_PREFIX)
	make gprclean
