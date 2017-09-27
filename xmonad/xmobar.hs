Config { font = "xft:Knack Nerd Font:size=8,FontAwesome:size=9"
       , bgColor = "#000000"
       , fgColor = "#ffffff"
       , position = Top
       , lowerOnStart = True
       , commands = [ Run MultiCpu [ "-t", "<fc=#ff79c6>\xf080 <total>%</fc>"
                                   , "-H", "50"
                                   , "--high", "#dc322f"
                                   ] 10
                    , Run Memory [ "-t", "<fc=#bd93f9>\xf2db <usedratio>%</fc>"
                                 , "-H", "80"
                                 , "--high", "#dc322f"
                                 ] 10
                    , Run DiskU [("/", "<fc=#f1fa8c>\xf200 <used>/<size></fc>")]
                                ["-L", "20", "-H", "50", "-m", "1", "-p", "3"] 20
                    , Run Volume "default" "Master" [ "-t", "\xf028 <volume>%" ] 10
                    , Run BatteryP ["BAT0", "BAT1"]
                                   [ "-t", "<fc=#8be9fd>\xf240 <left>%</fc>"
                                   , "-L", "10" , "-H", "80" , "-p", "3"
                                   , "--"
                                   , "-O" , "<fc=green>On</fc> - ", "-i", ""
                                   , "-o", "<fc=red>Off</fc> - "
                                   , "-L", "-15" , "-H", "-5"
                                   , "-l", "red" , "-m", "blue" , "-h", "green"
                                   ] 600
                    , Run Date "<fc=#ff5555>\xf073 %a %b %_d \xf017 %H:%M</fc>" "date" 10
                    , Run StdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " %StdinReader% }{ %multicpu%  %memory%  %disku%  %default:Master%  %battery%  %date% "
       }
