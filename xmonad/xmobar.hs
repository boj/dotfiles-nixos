Config { font = "xft:Knack Nerd Font:size=8,FontAwesome:size=9"
       , bgColor = "black"
       , fgColor = "white"
       , position = Top
       , lowerOnStart = True
       , commands = [ Run Network "enp0s3"
                                  [ "-t", "<fc=#859900><rx> \xf01a</fc> <fc=#dc322f><tx> \xf01b</fc>"
                                  ,"-S","True"
                                  ] 10
                    , Run DynNetwork [ "-t", "<fc=#859900><rx> \xf01a</fc> <fc=#dc322f><tx> \xf01b</fc>"
                                     , "-S","True"
                                     ] 10
                    , Run MultiCpu [ "-t", "\xf080 <total>%"
                                   , "-H", "50"
                                   , "--high", "#dc322f"
                                   ] 10
                    , Run Memory [ "-t", "\xf2db <usedratio>%"
                                 , "-H", "80"
                                 , "--high", "#dc322f"
                                 ] 10
                    , Run Date "\xf073 %a %b %_d \xf017 %H:%M " "date" 10
                    , Run BatteryP ["BAT0"]
                                   [ "-t", "\xf240 <left>%"
                                   , "-L", "10"
                                   , "-H", "80"
                                   , "-p", "3"
                                   , "--", "-O"
                                   , "<fc=green>On</fc> - "
                                   , "-o", ""
                                   , "-L", "-15"
                                   , "-H", "-5"
                                   , "-l", "red"
                                   , "-m", "blue"
                                   , "-h", "green"
                                   ] 600
                    , Run StdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader% }{ %dynnetwork% | %multicpu% %memory% %battery% | %date%"
       }
