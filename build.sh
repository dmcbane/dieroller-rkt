#! /usr/bin/bash
set -e

# memoize is no longer needed: the purchase table is built from combinations
# with repetition and cached with a promise instead of a memoized brute force.
raco exe dieroller.rkt
raco exe pathfinder-character.rkt
raco exe all_ability_scores.rkt
