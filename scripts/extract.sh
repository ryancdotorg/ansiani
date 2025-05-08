#!/bin/bash
set -uo pipefail
trap 's=$?; echo ": Error on line "$LINENO": $BASH_COMMAND"; exit $s' ERR
IFS=$'\n\t'

ffmpeg -i sneakers_main.vob -vf scale=720:405,pad=720:414 -q:v 8 /sata/ryanc/sneakers/frames/%08d.jpg
#ffmpeg -vsync 0 -i sneakers_main.vob -vf scale=720:405,pad=720:414 -r 1000 -q:v 8 -frame_pts true /dev/shm/frames/%012d.jpg
