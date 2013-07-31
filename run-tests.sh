#!/bin/sh -e
ECUKES=$(find elpa/ecukes-* -name "ecukes" | tail -1)
carton exec "$ECUKES" "$@"
