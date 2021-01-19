.PHONY: all
all: doc test

.PHONY: doc
doc: info/harmony-rules.info

# Variable that can be used to set additional flags to `lilypond` commands.
#
# Example: `make LILYPOND_FLAGS=-V …` activates debug mode of Lilypond.
LILYPOND_FLAGS=

GENERATE_DOC_DEPS=generate-doc.scm tools.scm graph.scm documentation.scm

info/harmony-rules.info: harmony-rules.scm generate-doc.ly $(GENERATE_DOC_DEPS)
	-rm -rf info
	mkdir info
	lilypond $(LILYPOND_FLAGS) generate-doc.ly
	makeinfo --info info/harmony-rules.texi -o info/

.PHONY:
test:
	lilypond $(LILYPOND_FLAGS) harmony-rules-tests.ly
