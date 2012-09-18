#!/bin/sh

cd "$(dirname "$0")"

exec ./util/ecukes/ecukes 2>/dev/null
