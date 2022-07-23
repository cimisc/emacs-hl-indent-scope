# SPDX-License-Identifier: GPL-2.0-or-later

# Can't use -batch because of no colors.
# test:
# 	@emacs -Q -l tests/hl-indent-scope-test.el -f hl-indent-scope-preview


test:
	@emacs -batch -l tests/hl-indent-scope-test.el -f ert-run-tests-batch-and-exit
