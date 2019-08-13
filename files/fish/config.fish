function fish_greeting
	set cmd (shuf -en 1 cowsay cowthink)
	#fortune -a; printf \n
	#fortune -a | $cmd -(shuf -en 1 b d g p s t w y) -f (shuf -en 1 /usr/share/cows/*.cow)
end

fish_vi_key_bindings

export EDITOR=/usr/bin/vim
export VISUAL=/usr/bin/vim

# Start X at login
if status is-login
	if test -z "$DISPLAY" -a $XDG_VTNR = 1
		exec startx -- -keeptty
	end
end

function ccd
	set d "$argv[1]"
	mkdir -p $d && cd $d
end
