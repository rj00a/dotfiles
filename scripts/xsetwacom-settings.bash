#!/usr/bin/env bash

# Map the tablet to the main monitor
# The other monitors would be HEAD-1, HEAD-2, etc.
xsetwacom set "Wacom Intuos Pro M Pen stylus" MapToOutput HEAD-0
xsetwacom set "Wacom Intuos Pro M Pen eraser" MapToOutput HEAD-0

