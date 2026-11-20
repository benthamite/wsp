#!/bin/sh
# Check wsp in clean batch Emacs: byte-compile every library and test file
# with warnings as errors, run checkdoc on the libraries, then run the ERT
# suites on the source and on the compiled code.  The tests build temporary
# wa-archive stores with invented data and talk to a fake wa-archive API on a
# temporary socket; they never open the real store or the real daemon.
set -eu

here=$(cd -- "$(dirname -- "$0")/.." && pwd -P)
build=$(mktemp -d "${TMPDIR:-/tmp}/wsp-check.XXXXXX")
trap 'find "$build" -depth -delete' EXIT
cd "$build"

libs=$(cd "$here" && ls wsp*.el 2>/dev/null | tr '\n' ' ')
tests=$(cd "$here" && ls test/*.el 2>/dev/null | tr '\n' ' ')
if [ -z "$libs" ]; then
  echo "run-tests.sh: no wsp libraries yet"
  exit 0
fi
quoted() {
  for f in "$@"; do printf ' "%s"' "$f"; done
}
# shellcheck disable=SC2086
lib_list=$(quoted $libs)
# shellcheck disable=SC2086
all_list=$(quoted $libs $tests)

"${EMACS:-emacs}" -Q --batch -L "$here" -L "$here/test" --eval "
(progn
  (require 'bytecomp)
  (let ((byte-compile-error-on-warn t)
        (byte-compile-dest-file-function
         (lambda (file)
           (expand-file-name (concat (file-name-nondirectory file) \"c\") \"$build\"))))
    (dolist (file '($all_list))
      (unless (byte-compile-file (expand-file-name file \"$here\"))
        (kill-emacs 1)))))"

"${EMACS:-emacs}" -Q --batch --eval "
(progn
  (require 'checkdoc)
  (setq checkdoc-verb-check-experimental-flag nil)
  (let ((problems 0))
    (advice-add 'display-warning :before
                (lambda (&rest _) (setq problems (1+ problems))))
    (dolist (file '($lib_list))
      (checkdoc-file (expand-file-name file \"$here\")))
    (unless (zerop problems)
      (message \"checkdoc: %d problems\" problems)
      (kill-emacs 1))
    (message \"checkdoc: clean\")))"

suites=""
for t in $tests; do
  case "$t" in
    *-test.el) suites="$suites -l $(basename "$t" .el)" ;;
  esac
done
# shellcheck disable=SC2086
"${EMACS:-emacs}" -Q --batch -L "$here" -L "$here/test" $suites -f ert-run-tests-batch-and-exit
# shellcheck disable=SC2086
"${EMACS:-emacs}" -Q --batch -L "$build" -L "$here" -L "$here/test" $suites \
  -f ert-run-tests-batch-and-exit
