# SPDX-License-Identifier: GPL-2.0-or-later

# Can't use -batch because of no colors.
test:
	@emacs -Q -l tests/hl-scope-test.el -f hl-scope-preview
