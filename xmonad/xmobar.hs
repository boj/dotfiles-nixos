Config { font = "xft:Hack:size=8:antialias=true"
       , bgColor = "black"
       , fgColor = "white"
       , position = Top
       , lowerOnStart = True
       , commands = [ Run Network "enp0s3"
                                  [ "-t", "<fc=#859900><rx> ↓</fc> <fc=#dc322f><tx> ↑</fc>"
                                  ,"-S","True"
                                  ] 10
                    , Run DynNetwork [ "-t", "<fc=#859900><rx> ↓</fc> <fc=#dc322f><tx> ↑</fc>"
                                     , "-S","True"
                                     ] 10
                    , Run MultiCpu [ "-t", "C: <total>%"
                                   , "-H", "50"
                                   , "--high", "#dc322f"
                                   ] 10
                    , Run Memory [ "-t", "M: <usedratio>%"
                                 , "-H", "80"
                                 , "--high", "#dc322f"
                                 ] 10
                    , Run Date "%_d %b %H:%M" "date" 10
                    , Run BatteryP ["BAT0"]
                                   [ "-t", "B: <left>%"
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
       , template = "%StdinReader% }{ %battery% %multicpu% %memory% | %dynnetwork% | %date%"
       }
