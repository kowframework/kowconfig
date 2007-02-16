# Makefile for the AW_Config
#
# @author Marcelo Coraça de Freitas <marcelo.batera@gmail.com> 


libs:
	ADA_PROJECT_PATH=.:../awlib gnatmake -P awconfig.gpr

parsers: libs
	ADA_PROJECT_PATH=.:../awlib gnatmake -P awconfig-parsers.gpr `xmlada-config`

tests: parsers
	ADA_PROJECT_PATH=.:../awlib gnatmake -P awconfig-tests.gpr



all: libs


run-xml: tests
	TEST_CONFIG_PATH=$(PWD)/data ./bin/xml-test


run-plain: tests
	TEST_CONFIG_PATH=$(PWD)/data ./bin/plain-test

run: run-xml run-plain


clean-libs:
	ADA_PROJECT_PATH=.:../awlib gnatclean -P awconfig.gpr
clean-parsers:	
	ADA_PROJECT_PATH=.:../awlib gnatclean -P awconfig-parsers.gpr
clean-tests:
	ADA_PROJECT_PATH=.:../awlib gnatclean -P awconfig-tests.gpr

clean: clean-tests clean-parsers clean-libs 
	@rm bin/* lib/* obj/*
	@echo "All clean"



docs:
	@-./gendoc.sh
	@echo "The documentation is generated by a bash script. Then it might fail in other platforms"
