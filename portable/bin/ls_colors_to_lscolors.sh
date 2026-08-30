#!/bin/bash
if [ -z "${LS_COLORS}" ] ; then
    echo "ExGxFxdaBxDaDaabababab"
    exit
fi
printf '%s\n' "${LS_COLORS}" \
    | awk 'BEGIN {
        FS = ":"

        keys[1] = "di"  # directory
        keys[2] = "ln"  # symbolic link
        keys[3] = "so"  # socket
        keys[4] = "pi"  # pipe/FIFO
        keys[5] = "ex"  # executable
        keys[6] = "bd"  # block device
        keys[7] = "cd"  # character device
        keys[8] = "su"  # setuid executable
        keys[9] = "sg"  # setgid executable
        keys[10] = "tw"  # sticky + other-writable dir
        keys[11] = "ow"  # other-writable dir

        colors["di"] = "01;32"  # Cx: bold green, default background
        colors["ln"] = "30;47"  # ah: black on white/light gray
        colors["so"] = "30;45"  # af: black on magenta
        colors["pi"] = "30;45"  # af: black on magenta
        colors["ex"] = "01;31"  # Bx: bold red, default background
        colors["bd"] = "30;46"  # ag: black on cyan
        colors["cd"] = "30;46"  # ag: black on cyan
        colors["su"] = "30;41"  # ab: black on red
        colors["sg"] = "30;41"  # ab: black on red
        colors["tw"] = "30;41"  # ab: black on red
        colors["ow"] = "30;41"  # ab: black on red

        mappings["0"] = "a"
        mappings["1"] = "b"
        mappings["2"] = "c"
        mappings["3"] = "d"
        mappings["4"] = "e"
        mappings["5"] = "f"
        mappings["6"] = "g"
        mappings["7"] = "h"
        mappings["9"] = "x"
    }

    {
        for (i = 1; i <= NF; i++) {
            split($i, entry, "=")
            colors[entry[1]] = entry[2]
        }
    }

    END {
        for (i = 1; i <= 11; i++) {
            key = keys[i]
            color = colors[key]

            background = "x"
            foreground = "x"
            bold = 0
            count = split(color, parts, ";")
            for (j = 1; j <= count; j++) {
                part = parts[j]
                first = substr(part, 1, 1)
                second = substr(part, 2, 1)
                if (part == "00") {
                    background = "x"
                    foreground = "x"
                    bold = 0
                } else if (part == "01") {
                    bold = 1
                } else if (first == "3") {
                    foreground = mappings[second]
                } else if (first == "4") {
                    background = mappings[second]
                }
            }
            if (foreground == "") {
                foreground = "x"
            }
            if (background == "") {
                background = "x"
            }
            if (bold && foreground != "x") {
                foreground = toupper(foreground)
            }
            printf "%s%s", foreground, background
        }
        print ""
    }
'
