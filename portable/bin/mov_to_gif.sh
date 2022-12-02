#!/bin/bash
INPUT="$1"
FRAMERATE=5
PALETTE=$(mktemp --suffix=.png)
INTERMEDIATE=$(mktemp --suffix=.gif)
OUTPUT="${INPUT%.*}.gif"
FILTERS="fps=${FRAMERATE},scale=160:-1:flags=lanczos"

ffmpeg -i "${INPUT}" -vf "${FILTERS},palettegen=max_colors=32" -y "${PALETTE}"
ffmpeg -i "${INPUT}" -i "${PALETTE}" -lavfi "${FILTERS} [x]; [x][1:v] paletteuse" -r "${FRAMERATE}" -y "${INTERMEDIATE}"
gifsicle -O3 --threads=8 -o "${OUTPUT}" "${INTERMEDIATE}"

rm -f "${PALETTE}"
rm -f "${INTERMEDIATE}"
