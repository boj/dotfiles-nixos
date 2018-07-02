##
## Red Phoenix (dkeg) - adapted by boj
##

%sh{
    black="rgb:000000"
    red="rgb:cc6666"
    blue="rgb:81a2be"

    orange1="rgb:F2361E"
    orange2="rgb:ED4B19"
    orange3="rgb:FA390F"
    light_orange1="rgb:DF9767"
    white1="rgb:EDEDED"
    white2="rgb:E1E1E1"
    gray1="rgb:6F6F6F"
    gray2="rgb:D1D1D1"
    gray3="rgb:2D2D2D"
    gray4="rgb:909090"
    tan1="rgb:D2C3AD"
    tan2="rgb:AAA998"
    tan3="rgb:DF9767"
    yellow1="rgb:AAA998"
    purple1="rgb:4C3A3D"

    foreground=${white1}
    background=${black}
    selection=${tan3}
    window=${gray3}
    text=${white2}
    text_light=${white1}
    line=${tan1}
    comment=${gray1}

    ## code
    echo "
        face value ${orange2}
        face type ${gray2}
        face variable ${orange1}
        face module ${gray2}
        face function ${yellow1}
        face string ${tan2}
        face keyword ${light_orange1}
        face operator ${yellow1}
        face attribute ${tan1}
        face comment ${gray1}
        face meta ${gray2}
        face builtin ${tan1}
    "

    ## markup
    echo "
        face title blue
        face header ${orange1}
        face bold ${orange2}
        face italic ${orange3}
        face mono ${yellow1}
        face block ${tan1}
        face link blue
        face bullet ${gray1}
        face list ${gray1}
    "

    ## builtin
    echo "
        face Default ${text},${background}
        face PrimarySelection default,${selection}
        face SecondarySelection default,${selection}
        face PrimaryCursor black,${tan1}
        face SecondaryCursor black,${tan2}
        face PrimaryCursorEol black,${orange2}
        face SecondaryCursorEol black,${orange2}
        face LineNumbers ${text_light},${background}
        face LineNumberCursor ${text},${gray1}+b
        face MenuForeground ${text_light},blue
        face MenuBackground ${orange1},${window}
        face MenuInfo ${gray1}
        face Information white,${window}
        face Error white,${red}
        face StatusLine ${text},${window}
        face StatusLineMode ${yellow1}+b
        face StatusLineInfo ${orange2}
        face StatusLineValue ${orange2}
        face StatusCursor ${window},${orange2}
        face Prompt ${background},${orange2}
        face MatchingChar ${orange3},${background}+b
        face BufferPadding ${orange2},${background}
    "
}
