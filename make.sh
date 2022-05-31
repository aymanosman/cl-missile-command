#!/bin/sh
set -e

sbcl --noinform \
     --non-interactive \
     --load release.lisp
