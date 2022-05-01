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

GENERATE_DOC_DEPS=generate-doc.scm tools.scm graph.scm documentation.scm

info/harmony-rules.info: harmony-rules.scm generate-doc.ly $(GENERATE_DOC_DEPS)
	-rm -rf info
	mkdir info
	lilypond $(LILYPOND_FLAGS) generate-doc.ly
	makeinfo --info info/harmony-rules.texi -o info/

.PHONY:
test:
	lilypond $(LILYPOND_FLAGS) harmony-rules-tests.ly

.PHONY:
clean:
	-rm -rf info
