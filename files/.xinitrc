
# Load xresources
xrdb ~/.Xresources &

# Swap caps lock and escape keys
setxkbmap -option 'caps:swapescape' &

# Adjust mouse pointer speed
# NOTE: Previously I had tried modifying the "Coordinate Transformation Matrix", but that didn't
# really work.
# '14' is the mouse pointer ID, '297' is the property ID, and '-0.5' is the acceleration factor.
# You can identify devices by running 'xinput' without any arguments.
# I think this would break if the hardware configuration changes.
# TODO: what is '-0.45'?
xinput set-prop 14 297 -0.45 &

# set keyboard repeat delay and rate.
# '250' is the delay before autorepeat begins.
# '40' is the repeat rate.
xset r rate 250 40 &

# Load desktop wallpaper
feh --no-fehbg --bg-fill ~/shared/wallpaper.jpg &

# Unclutter hides the mouse cursor when it hasn't been used in a while.
unclutter &

# Start polybar with the specified bar
polybar main &

# Start the NetworkManager applet
nm-applet &

# Start the Simple X hotkey daemon
sxhkd &

# start bspwm
exec bspwm
