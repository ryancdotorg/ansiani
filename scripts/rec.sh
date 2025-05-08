#!/bin/bash
set -uo pipefail
trap 's=$?; echo ": Error on line "$LINENO": $BASH_COMMAND"; exit $s' ERR
IFS=$'\n\t'

ffmpeg -video_size 484x316 -framerate 24 -f x11grab -i :0.0+38,51 -vcodec libx264 -crf 0 -preset ultrafast -t 10 /dev/shm/rec.mp4
