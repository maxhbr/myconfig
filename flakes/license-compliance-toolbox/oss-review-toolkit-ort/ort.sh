#!/usr/bin/env bash
# Copyright 2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -euo pipefail

DEBUG_LEVEL="--info"
DOCKERIZE_BY_DEFAULT=false
baseTag=ort:latest
tag=myort:latest
ort=ort
orth=orth

help() {
    cat <<EOF
    $0 inputFileOrDir [other [ort [args]]]

e.g.
    $0 [--dockerize] short /path/to/folder
    $0 [--dockerize] all /path/to/folder -m gradle
    $0 [--dockerize] analzye /path/to/folder
    $0 [--dockerize] analzye /path/to/folder -m gradle
    $0 [--dockerize] scan /path/to/folder/analyzer-result.json
    $0 ...
    $0 rm-docker-images
EOF
}

################################################################################
##  prepare home .ort folder  ##################################################
################################################################################

prepareDotOrt() {
    declare -a types=("analyzer" "downloader" "scanner" "config" )
    for type in ${types[@]}; do
        mkdir -p "$HOME/.ort/dockerHome/.ort/$type"
        if [[ ! -e "$HOME/.ort/$type" ]]; then
            ln -s "$HOME/.ort/dockerHome/.ort/$type" "$HOME/.ort/$type"
        fi
    done
    cat <<EOF > "$HOME/.ort/dockerHome/.ort/config/ort.conf"
ort {
  analyzer {
    allowDynamicVersions = true
  }
  scanner {
    storages {
      clearlyDefined {
        serverUrl = "https://api.clearlydefined.io"
      }
      fileBasedStorage {
        backend {
          localFileStorage {
            directory = "${HOME}/.ort/scanner/scan-results"
            compression = false
          }
        }
      }
    }

    storageReaders: [
      "fileBasedStorage"
    ]

    storageWriters: [
      "fileBasedStorage"
    ]
  }
}
EOF
}

################################################################################
##  stuff related to dockerizing  ##############################################
################################################################################

buildImageIfMissing() {
    export DOCKER_BUILDKIT=1

    if [[ "$(docker images -q $tag 2> /dev/null)" == "" ]]; then
        if [[ "$(docker images -q $baseTag 2> /dev/null)" == "" ]]; then
            ORT=$(mktemp -d)
            trap "rm -rf $ORT" EXIT
            git clone https://github.com/oss-review-toolkit/ort $ORT

            docker build \
                --network=host \
                -t $baseTag $ORT
        else
            >&2 echo "docker base image already build, at $(docker inspect -f '{{ .Created }}' $baseTag)"
        fi
        docker build -t $tag -<<EOF
FROM $baseTag
ENV JAVA_OPTS "-Xms2048M -Xmx16g -XX:MaxPermSize=4096m -XX:MaxMetaspaceSize=4g"
RUN set -x \
 && ssh-keygen -b 2048 -t rsa -f ~/.ssh/id_rsa -q -N ""
EOF
    else
        echo "docker image already build, at $(docker inspect -f '{{ .Created }}' $tag)"
    fi
}

runDockerizedOrt() {
    local input="$1"; shift
    local output="$1"; shift
    mkdir -p "$output"

    buildImageIfMissing

    local inputFile
    local inputDir
    if [[ -f "$input" ]]; then
        inputFile="$(basename "$input")"
        inputDir="$(dirname "$input")"
    else
        inputFile=""
        inputDir="$input"
    fi

    local dockerArgs=("-i" "--rm")
    dockerArgs+=("-v" "/etc/group:/etc/group:ro" "-v" "/etc/passwd:/etc/passwd:ro" "-u" "$(id -u $USER):$(id -g $USER)")
    dockerArgs+=("-v" "$HOME/.ort/dockerHome:$HOME")
    dockerArgs+=("-v" "$inputDir:/workdir")
    dockerArgs+=("-v" "$output:/out")
    dockerArgs+=("-w" "/workdir")
    dockerArgs+=("--net=host")
    local args=($(echo "${@}" | sed "s%$output%/out%g" | sed "s%$inputDir%/workdir%g"))

    (set -x;
     docker run \
         "${dockerArgs[@]}" \
         "$tag" \
         "${args[@]}" -i "/workdir/$inputFile" -o /out;
     >&2 times
     )
}

################################################################################
##  actual main function that composes args  ###################################
################################################################################

runOrt() {
    local dockerize=$DOCKERIZE_BY_DEFAULT
    local task="$1"; shift

    local input="$(readlink -f "$1")"; shift
    if [[ ! -e "$input" ]]; then
        echo "the input=$input is missing"
        return 1
    fi

    local output="$input"
    if [[ -f "$output" ]]; then
        output="$(dirname $output)"
    fi
    output="${output%_ort}_ort"
    mkdir -p "$output"

    local args=("--force-overwrite" "$DEBUG_LEVEL")
    case $task in
        analyze)
            args+=("-P" "ort.analyzer.allowDynamicVersions=true" "analyze" "--clearly-defined-curations" "--output-formats" "JSON,YAML")
            ;;

        download)
            args+=("download")
            ;;

        scan)
            args+=("scan")
            dockerize=true
            ;;

        report)
            args+=("report" "-f" "StaticHtml,WebApp,Excel,NoticeTemplate,SPDXDocument,GitLabLicensemodel,AsciiDocTemplate,CycloneDx,EvaluatedModel")
            if [[ -f "$output/resolutions.yml" ]]; then
                args+=("--resolutions-file" "$output/resolutions.yml")
            fi
            ;;

        *)
            args+=("$task")
            ;;
    esac
    args+=("${@}")

    cat <<EOF >> "$output/_calls"
[$(date)] ort
    -i "$(realpath --relative-to="$(pwd)" "$input")"
    -o "$(realpath --relative-to="$(pwd)" "$output")"
    ${args[@]}
EOF

    if [[ "$dockerize" == "true" ]]; then
        runDockerizedOrt "${input}" "${output}" "${args[@]}"
    else
        args+=("-i" "${input}" "-o" "$output")
        (set -x;
            $ort "${args[@]}";
            >&2 times)
    fi
}


################################################################################
##  wrapper to run a more complete pipeline  ###################################
################################################################################

doAll() {
    local input="$(readlink -f "$1")"; shift
    if [[ ! -e "$input" ]]; then
        echo "the input=$input is missing"
        return 1
    fi

    local output="$input"
    if [[ -f "$output" ]]; then
        output="$(dirname $output)"
    fi
    output="${output%_ort}_ort"

    local reportResult="$output/scan-report-web-app.html"
    if [[ ! -f "$reportResult" ]]; then
        local scanResult="$output/scan-result.yml"
        local analyzeResult="$output/analyzer-result.yml"
        if [[ ! -f "$scanResult" ]]; then
            if [[ ! -f "$analyzeResult" ]]; then
                runOrt analyze "$input" "$@" || [[ -f "$analyzeResult" ]]
            else
                echo "skip analyze ..."
            fi
            runOrt scan "$analyzeResult" || [[ -f "$scanResult" ]]
        else
            echo "skip scan ..."
        fi
        runOrt report "$scanResult" || runOrt report "$analyzeResult"
    else
        echo "skip report ..."
    fi
}

doShort() {
    local input="$(readlink -f "$1")"; shift
    if [[ ! -e "$input" ]]; then
        echo "the input=$input is missing"
        return 1
    fi

    local output="$input"
    if [[ -f "$output" ]]; then
        output="$(dirname $output)"
    fi
    output="${output%_ort}_ort"



    local reportResult="$output/scan-report-web-app.html"
    if [[ ! -f "$reportResult" ]]; then
        local analyzeResult="$output/analyzer-result.yml"
        if [[ ! -f "$analyzeResult" ]]; then
            runOrt analyze "$input" "$@" || [[ -f "$analyzeResult" ]]
        else
            echo "skip analyze ..."
        fi
        runOrt report "$analyzeResult"
    else
        echo "skip report ..."
    fi
}

doListPackages() {
    local input="$(readlink -f "$1")"; shift

    buildImageIfMissing

    local inputFile
    local inputDir
    if [[ -f "$input" ]]; then
        inputFile="$(basename "$input")"
        inputDir="$(dirname "$input")"
    else
        exit 1
    fi

    local dockerArgs=("-i" "--rm")
    dockerArgs+=("-v" "$inputDir:/workdir")
    dockerArgs+=("-w" "/workdir")
    dockerArgs+=("--net=host")
    dockerArgs+=("--entrypoint" "/opt/ort/bin/orth")
    local args=($(echo "${@}" | sed "s%$inputDir%/workdir%g"))
    (set -x;
     docker run \
         "${dockerArgs[@]}" \
         "$tag" \
         list-packages "${args[@]}" --ort-file "/workdir/$inputFile";
     >&2 times
     )
}

################################################################################
##  handle arguments and run  ##################################################
################################################################################

prepareDotOrt

if ! command -v $ort &> /dev/null; then
    (>&2 echo "$ort not in \$PATH, dockerize by default")
    DOCKERIZE_BY_DEFAULT=true
    if [[ "$1" == "--dockerize" ]]; then
        shift
    fi
else
    if [[ "$1" == "--dockerize" ]]; then
        DOCKERIZE_BY_DEFAULT=true
        shift
    fi
fi

if [[ $# = 0 ]]; then
    help
elif [[ "$1" == "rm-docker-images" ]]; then
    docker rmi "$tag"
    docker rmi "$baseTag"
elif [[ "$1" == "all" ]]; then
    shift
    doAll "$@"
elif [[ "$1" == "short" ]]; then
    shift
    doShort "$@"
elif [[ "$1" == "list-packages" ]]; then
    shift
    doListPackages "$@"
else
    runOrt "$@" || help
fi
>&2 times
