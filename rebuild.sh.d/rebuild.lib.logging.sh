. ./common.sh

setupLoging() {
    local targetHost="$1"

    mkdir -p "$logsDir"
    local logfile="$logsDir/$(date +%Y-%m-%d)-myconfig-${targetHost}.log"
    echo -e "\n\n\n\n\n\n\n" >> $logfile
    exec &> >(tee -a $logfile)
}

export -f setupLoging
