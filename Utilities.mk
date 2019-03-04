TO_UPPER=$(shell echo '$1' | tr a-z A-Z)
TO_LOWER=$(shell echo '$1' | tr A-Z a-z)

# Row-major: { for(x:$1) for(y:$2) x(y) }
DO_EA=$(foreach L,$1,$(call $L,$2))
# Composed row-major: { ... for(z:$3) x(y(z)) }
DO_EA2=$(call DO_EA,$1,$(call DO_EA,$2,$3))

# Col-major: { for(y:$2) for(x:$1) x(y) }
XDO_EA=$(foreach X,$2,$(call DO_EA,$1,$X))
# Composed col-major: { for(z:$3) ... x(y(z)) }
XDO_EA2=$(foreach X,$(call XDO_EA,$2,$3),$(call DO_EA,$1,$X))

# Maps one file path to another through patterns
REPAT=$(call PAT_$2,$(call UNPAT_$1,$3))
# Maps one file path to several
REPAT_EA=$(call DO_EA2,$(2:%=PAT_%),UNPAT_$1,$3)

# Convenience compositions with wildcard
WPAT=$(wildcard $(call PAT_$1,$2))
WREPAT=$(wildcard $(call REPAT,$1,$2,$3))
WDO_EA=$(wildcard $(call DO_EA,$1,$2))
WDO_EA2=$(wildcard $(call DO_EA2,$1,$2,$3))
WREPAT_EA=$(wildcard $(call REPAT_EA,$1,$2,$3))

# Splits one patterned file into a file pair
#   Note - this still allows identities like PAT_EXE/UNPAT_EXE
CROSS=$(foreach X,$4,$(call REPAT,$1,$2,$X):$(call REPAT,$1,$3,$X))
# Identical to 'pkg-config $1 $2' except for '$2'=='';
#   '--silence-errors' and redirection just swap false- / false+.
PKG_CONFIG=$(foreach N,$2,$(shell pkg-config $1 $N))
CONFIG_O=$(call PKG_CONFIG,--cflags,$1)
CONFIG_SO=$(call PKG_CONFIG,--libs,$1)

# Examples - uncomment and make 'vars-TESTS'

# Test 'do each' functors (different orders, +/- wildcard)
$(eval TEST1=$(call DO_EA,PAT_HPP PAT_CPP,$$(NAMES_SO)))
$(eval TEST2=$(call WDO_EA,PAT_HPP PAT_CPP PAT_APP,$$(NAMES_SO)))
$(eval TEST3=$(call XDO_EA,PAT_HPP PAT_CPP,$$(NAMES_SO)))
# Test composed 'do each' functors
$(eval TEST4=$(call DO_EA2,dir notdir,PAT_APP PAT_HPP,$$(NAMES_SO)))
$(eval TEST5=$(call XDO_EA2,dir notdir,PAT_APP PAT_HPP,$$(NAMES_SO)))
# Test zip (pairwise foreach) - 6 and 7 should be the same
$(eval TEST6=$(call MERGE,$(COMMA) ,$(join $$(FILES_EXE:%=%:),$$(SRCS_EXE))))
$(eval TEST7=$(call MERGE,$(COMMA) ,$(call CROSS,SO,CPP,HPP,$$(FILES_SO))))
# Test composed foreach, pattern, and unpattern (+/- wildcard)
$(eval TEST8=$(call REPAT_EA,SO,HPP TPP,$$(FILES_SO)))
$(eval TEST9=$(call WREPAT_EA,SO,HPP TPP,$$(FILES_SO)))
# Aggregate for use with target 'vars-TESTS'; change numbers here,
#   or make 'var-TEST{1..9}', 'var-TEST{2,4,6}', etc.
$(eval TESTS=$(foreach N,1 3 6 7 8 9,TEST$N))


SOURCE=$(shell echo $(shell cat $(wildcard $(call PAT_D,$1)) $(NULL)))
EXTRACT=$(filter-out $(call PAT_HPP,$1) $(call PAT_CPP,$1),$(call SOURCE,$1))
CPP_EXTRACT=$(call WREPAT,HPP,CPP,$(call EXTRACT,$1))
D_EXTRACT=$(call REPAT,CPP,D,$(call CPP_EXTRACT,$1))
O_EXTRACT=$(call REPAT,CPP,O,$(call CPP_EXTRACT,$1))
SO_EXTRACT=$(call REPAT,CPP,SO,$(call CPP_EXTRACT,$1))
LD_EXTRACT=$(addprefix -l,$(call XDO_EA,UNPAT_CPP,$(call CPP_EXTRACT,$1)))
