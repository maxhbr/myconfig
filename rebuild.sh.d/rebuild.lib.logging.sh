. ../common.sh

setupLoging() {
    mkdir -p "${myconfigDir}/_logs/"
    logfile="${myconfigDir}/_logs/$(date +%Y-%m-%d)-myconfig.log"
    echo -e "\n\n\n\n\n\n\n" >> $logfile
    exec &> >(tee -a $logfile)
}

export -f setupLoging
