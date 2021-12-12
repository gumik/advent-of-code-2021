#!/usr/bin/env bash

ANSWERS_WEB='/tmp/aoc2021-answers-web.txt'
ANSWERS_CODE='/tmp/aoc2021-answers-code.txt'

for file in puzzle/*; do grep 'Your puzzle answer' $file; done | sed -e 's/Your puzzle answer was //' | sed -e 's/\.//' > ${ANSWERS_WEB}

stack run | grep -v day | grep -v solution | sed -e 's/ *//' > ${ANSWERS_CODE}

diff -u ${ANSWERS_WEB} ${ANSWERS_CODE}

rm ${ANSWERS_WEB} ${ANSWERS_CODE}
