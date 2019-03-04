# Tools
doxygen?=doxygen
doxymake=$(doxygen) -g

# Input parameters
DIR_DOC?=doc/
DIR_TAG?=$(DIR_DOC)tags/

HTML_EXTRA_STYLESHEET?=doc/style.css
BUILTIN_STL_SUPPORT?=YES
COLS_IN_ALPHA_INDEX?=5
DOT_TRANSPARENT?=YES
EXCLUDE_PATTERNS?=*.o, *.d, *.h, */obj/*
EXTENSION_MAPPING=tpp=C++
EXTRACT_ALL?=YES
EXTRACT_PRIVATE?=YES
EXTRACT_PACKAGE?=YES
FILE_PATTERNS?=*.hpp *.tpp *.cpp *.ipp *.markdown *.md
FULL_PATH_NAMES=YES
INCLUDE_PATH=$(DIR_HDR)
#INPUT=include/ src/ app/
INPUT=$(DIR_HDR) $(DIR_SRC) $(DIR_APP)
#INPUT=$(foreach D,HDR SRC APP,$(foreach P,$(FILE_PATTERNS),$(wildcard $D$P $D*/$P)))
INPUTS=$(INPUT)
OUTPUT_DIRECTORY=$(DIR_DOC)
QT_AUTOBRIEF?=YES
MULTILINE_CPP_IS_BRIEF=YES
RECURSIVE?=YES
SHOW_INCLUDE_FILES?=NO
STRIP_FROM_PATH?=./
STRIP_FROM_INC_PATH?=include/
WARNINGS=NO

#TAG_DIRNAMES?=html latex
TAG_DIRNAMES?=html latex rtf man
TAG_FILENAMES:=$(TAG_DIRNAMES:%=$(DIR_DOC)%.tag)
TAG_FILEMAPS:=$(join $(TAG_FILENAMES),$(addprefix =,$(TAG_FILENAMES:.tag=)))
DOXY_FILENAMES?=$(addprefix $(DIR_DOC)Doxyfile_,$(TAG_DIRNAMES))
DOXY_FILENAMES:=$(DOXY_FILENAMES) $(DOXY_FILENAMES:%=%.bak)

#DOXY_LOCATE=$(foreach D,HDR SRC APP,$(foreach X,.hpp .tpp .cpp,$(wildcard $(DIR_$D)*$X)))
DOXY_LOCATE:=$(wildcard $(DIR_HDR)*.hpp $(DIR_HDR)*.tpp $(DIR_SRC)*.cpp $(DIR_APP)*.cpp)
DOXY_LOCATE:=$(DOXY_LOCATE:/home/john/dev/geometric/include/=)

$(DIR_DOC)%.tag: $(DOXY_LOCATE)
	$(doxymake) $(DIR_DOC)Doxyfile_$* >$(NULL)
	$(eval DOXY_VARS:=HTML_EXTRA_STYLESHEET BUILTIN_STL_SUPPORT COLS_IN_ALPHA_INDEX \
		DOT_TRANSPARENT EXCLUDE_PATTERNS EXTRACT_ALL EXTRACT_PRIVATE EXTRACT_PACKAGE \
		FULL_PATH_NAMES INCLUDE_PATH INPUT OUTPUT_DIRECTORY QT_AUTOBRIEF MULTILINE_CPP_IS_BRIEF \
		RECURSIVE STRIP_FROM_PATH STRIP_FROM_INC_PATH WARNINGS)
	@echo $(foreach VAR,$(DOXY_VARS),"$(VAR)=$($(VAR))\n") \
		GENERATE_TAGFILE="$(DIR_DOC)$*.tag\n" \
		GENERATE_$(call TO_UPPER,$*)="YES\n" \
		$(addprefix GENERATE_,$(foreach N,$(TAG_DIRNAMES),\
		$(addsuffix ="NO\n",$(call TO_UPPER,$(filter-out $*,$N))))) \
		>>$(DIR_DOC)Doxyfile_$* && $(doxygen) $(DIR_DOC)Doxyfile_$* >$(NULL)
	$(doxygen) $(DIR_DOC)Doxyfile_$* >$(NULL)

$(TAG_DIRNAMES): %: $(DIR_DOC)%.tag
doc: $(TAG_FILENAMES)

clean-doc:; $(RM) -R $(DOXY_FILENAMES) $(TAG_FILENAMES) $(TAG_DIRNAMES:%=$(DIR_DOC)%/*)

.PHONY: doc doxy-reset-% clean-doc $(TAG_DIRNAMES)
