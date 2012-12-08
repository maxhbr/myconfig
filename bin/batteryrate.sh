#!/bin/sh

BatPresent=$(acpi -b | wc -l)
ACPresent=$(acpi -a | grep -c on-line)

if [[ $BatPresent == "0" ]]; then
  echo -n "noBat"
else
  echo -n "Bat: "
  PERC=$(acpi -b | awk '{print $4}' | tr -d "%,")
  if [[ $ACPresent == "0" ]]; then
    TIME=$(acpi -b | awk '{print $5}' | tr -d "%,")
    if [[ $TIME == 00:* ]]; then
      if [[ $TIME == 00:0* ]]; then
        #zenity --error --text $TIME
        notify-send -u critical $TIME
        echo -n "<fc=#000000,#ffff00> ${PERC}% ${TIME} </fc>"
      elif [[ $TIME == 00:1* ]]; then
        echo -n "<fc=#000000,#ffff00> ${PERC}% ${TIME} </fc>"
      elif [[ $TIME == 00:2* ]]; then
        echo -n "<fc=#000000,#ffff00> ${PERC}% ${TIME} </fc>"
      else
        echo -n "<fc=#ee0000> ${PERC}% ${TIME} </fc>"
      fi
    else
      echo -n "<fc=#ee9a00>${PERC}% ${TIME}</fc>"
    fi
  else
    echo -n "${PERC}%"
  fi
fi
