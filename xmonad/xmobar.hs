Config { font = "xft:Knack Nerd Font:size=8,FontAwesome:size=9"
       , bgColor = "#131313"
       , fgColor = "#ffffff"
       , position = Top
       , lowerOnStart = True
       , commands = [ Run DynNetwork [ "-t", "<fc=green>\xf01a <rx></fc> <fc=red>\xf01b <tx></fc>"
                                     , "-S","True"
                                     ] 10
                    , Run MultiCpu [ "-t", "<fc=#df9767>\xf080 <total>%</fc>"
                                   , "-H", "50"
                                   , "--high", "#dc322f"
                                   ] 10
                    , Run Memory [ "-t", "<fc=#d2c3ad>\xf2db <usedratio>%</fc>"
                                 , "-H", "80"
                                 , "--high", "#dc322f"
                                 ] 10
                    , Run DiskU [("/", "<fc=#f2361e>\xf200 <used>/<size></fc>")]
                                ["-L", "20", "-H", "50", "-m", "1", "-p", "3"] 20
                    , Run Volume "default" "Master" [ "-t", "\xf028 <volume>%" ] 10
                    , Run BatteryP ["BAT0"]
                                   [ "-t", "<fc=#aaa998>\xf240 <left>%</fc>"
                                   , "-L", "10" , "-H", "80" , "-p", "3"
                                   , "--"
                                   , "-O" , "<fc=green>On</fc> - ", "-i", ""
                                   , "-o", "<fc=red>Off</fc> - "
                                   , "-L", "-15" , "-H", "-5"
                                   , "-l", "red" , "-m", "blue" , "-h", "green"
                                   ] 600
                    , Run Date "<fc=#f2361e>\xf073 %a %b %_d</fc> <fc=#b1b1b1>\xf017 %H:%M</fc>" "date" 10
                    , Run StdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " %date% : %StdinReader% }{ %dynnetwork%  %multicpu%  %memory%  %disku%  %default:Master%  %battery% "
       }
