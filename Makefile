# Lilypond Harmony Rules tests harmony rules of Lilypond scores.
# Copyright (C) 2021  Stéphane SOPPERA
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

.PHONY: all
all: doc test

.PHONY: doc
doc: info/harmony-rules.info

# Variable that can be used to set additional flags to `lilypond` commands.
#
# Example: `make LILYPOND_FLAGS=-V …` activates debug mode of Lilypond.
LILYPOND_FLAGS=

# Variable that can be used to set the GUILE intepreter to use.
GUILE=guile

# Variable that can be use to set additional flags to GUILE.
GUILE_FLAGS=

GENERATE_DOC_DEPS=docgen/generate.scm tools.scm graph.scm documentation.scm

# Note that we use GUILE_AUTO_COMPILE=1 to force auto-compilation so
# that function parameters are not lost with Guile 2.2. By default
# Lilypond uses a mode where sources are not compiled and in that case
# Guile can't extract the parameters's names correctly. See:
# https://lists.gnu.org/archive/html/bug-lilypond/2022-05/msg00000.html
info/harmony-rules.info: harmony-rules.scm generate-doc.ly $(GENERATE_DOC_DEPS)
	-rm -rf info
	mkdir info
	GUILE_AUTO_COMPILE=1 lilypond $(LILYPOND_FLAGS) generate-doc.ly
	makeinfo --info info/harmony-rules.texi -o info/

.PHONY: test
test: test-harmony-rules test-docgen

.PHONY: test-harmony-rules
test-harmony-rules:
	lilypond $(LILYPOND_FLAGS) harmony-rules-tests.ly

.PHONY: test-docgen
test-docgen:
	${GUILE} ${GUILE_FLAGS} -L . docgen/introspection-tests.scm
	${GUILE} ${GUILE_FLAGS} -L . docgen/module-map-tests.scm

.PHONY: clean
clean:
	-rm -rf info
