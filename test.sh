#!/usr/bin/env sh

set -o errexit
set -o nounset
set -o xtrace

readonly init_package_el="(progn \
  (require 'package) \
  (push '(\"melpa\" . \"http://melpa.org/packages/\") package-archives) \
  (package-initialize) \
  (unless package-archive-contents \
     (package-refresh-contents)) \
  (dolist (pkg '(package-lint)) \
    (unless (package-installed-p pkg) \
      (package-install pkg))))"

emacs \
    -Q \
    -batch \
    --eval "$init_package_el" \
    -l package-lint.el \
    --eval "(setq byte-compile-error-on-warn t)" \
    -f batch-byte-compile \
    center-layout.el

emacs \
    -Q \
    -batch \
    --eval "$init_package_el" \
    -l package-lint.el \
    -f package-lint-batch-and-exit \
    center-layout.el

emacs \
    -Q \
    -batch \
    --eval "$init_package_el" \
    -l center-layout.el \
    -l center-layout-test.el \
    -f ert-run-tests-batch-and-exit
