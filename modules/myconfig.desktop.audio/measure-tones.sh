#!/usr/bin/env nix-shell
#! nix-shell -i bash -p sox pipewire pulseaudio coreutils gawk graphviz gnused

set -euo pipefail

NAME="measurement"
MEASUREMENTS=5
DURATION=2.0
LEVEL=0.1
RATE=48000

FREQUENCIES=(
    30 40 50 60 70 80
    90 100 110 120 130 140 150
    175 200 250 300 350 440 600 800
)

MODES=(left right stereo)

OUTDIR="/tmp/measure-tones"
RECORD_LEAD=0.30
TONE_SETTLE=0.25
RECORD_TAIL=0.15
NORM_MIN=100
NORM_MAX=300
SILENCE_THRESHOLD_DBFS=-100
MIN_ACTIVE_FRACTION=0.50
MIN_RESPONSE_SPAN_DB=3.0
SOURCE_MODE="all"
CUSTOM_SOURCES=""

X_LEFT=120
X_RIGHT=1120
Y_BOTTOM=100
Y_TOP=700

usage() {
    cat <<EOF2
Usage: $0 [options]

Options:
  --name NAME
  --measurements N
  --duration SECONDS
  --level AMPLITUDE
  --frequencies "30 40 50 ..."
  --normalize MIN MAX
  --silence-threshold DBFS
  --min-response-span DB
  --sources all|default|"SOURCE1,SOURCE2,..."
  --list-sources
  --help

Each frequency/repetition is measured three times: left, right, and stereo.
The order of those three modes is randomized on every repetition. Frequency
repetitions are randomized as well.
EOF2
}

LIST_SOURCES=0
while [[ $# -gt 0 ]]; do
    case "$1" in
        --name) NAME="$2"; shift 2 ;;
        --measurements) MEASUREMENTS="$2"; shift 2 ;;
        --duration) DURATION="$2"; shift 2 ;;
        --level) LEVEL="$2"; shift 2 ;;
        --frequencies) read -r -a FREQUENCIES <<< "$2"; shift 2 ;;
        --normalize) NORM_MIN="$2"; NORM_MAX="$3"; shift 3 ;;
        --silence-threshold) SILENCE_THRESHOLD_DBFS="$2"; shift 2 ;;
        --min-response-span) MIN_RESPONSE_SPAN_DB="$2"; shift 2 ;;
        --sources)
            case "$2" in
                all) SOURCE_MODE="all"; CUSTOM_SOURCES="" ;;
                default) SOURCE_MODE="default"; CUSTOM_SOURCES="" ;;
                *) SOURCE_MODE="custom"; CUSTOM_SOURCES="$2" ;;
            esac
            shift 2
            ;;
        --list-sources) LIST_SOURCES=1; shift ;;
        --help|-h) usage; exit 0 ;;
        *) echo "Unknown argument: $1" >&2; usage >&2; exit 1 ;;
    esac
done

if (( MEASUREMENTS < 3 )); then
    echo "--measurements must be >= 3" >&2
    exit 1
fi

mkdir -p "$OUTDIR"
DEFAULT_SOURCE="$(pactl get-default-source)"
mapfile -t AVAILABLE_SOURCES < <(pactl list short sources | awk -F '\t' '$2 !~ /\.monitor$/ { print $2 }')

if (( LIST_SOURCES )); then
    echo "Available capture sources:"
    for source in "${AVAILABLE_SOURCES[@]}"; do
        if [[ "$source" == "$DEFAULT_SOURCE" ]]; then
            printf '  * %s [default]\n' "$source"
        else
            printf '    %s\n' "$source"
        fi
    done
    exit 0
fi

declare -a SOURCES=()
case "$SOURCE_MODE" in
    default) SOURCES=("$DEFAULT_SOURCE") ;;
    all)
        SOURCES+=("$DEFAULT_SOURCE")
        for source in "${AVAILABLE_SOURCES[@]}"; do
            [[ "$source" == "$DEFAULT_SOURCE" ]] && continue
            SOURCES+=("$source")
        done
        ;;
    custom) IFS=',' read -r -a SOURCES <<< "$CUSTOM_SOURCES" ;;
esac

declare -A SEEN_SOURCES
declare -a UNIQUE_SOURCES=()
for source in "${SOURCES[@]}"; do
    [[ -n "${SEEN_SOURCES[$source]:-}" ]] && continue
    SEEN_SOURCES[$source]=1
    UNIQUE_SOURCES+=("$source")
done
SOURCES=("${UNIQUE_SOURCES[@]}")

if [[ ${#SOURCES[@]} -eq 0 ]]; then
    echo "No capture sources found." >&2
    exit 1
fi

SAFE_NAME="$(printf '%s' "$NAME" | tr '[:space:]' '-' | tr -cd '[:alnum:]_.-')"
[[ -n "$SAFE_NAME" ]] || SAFE_NAME="measurement"
TIMESTAMP="$(date +%Y%m%d-%H%M%S)"
PREFIX="${SAFE_NAME}-${TIMESTAMP}"
CSV="${OUTDIR}/${PREFIX}.csv"
PNG="${OUTDIR}/${PREFIX}.png"
DOT="${OUTDIR}/${PREFIX}.dot"
TMPDIR="$(mktemp -d "${OUTDIR}/tmp.XXXXXX")"
cleanup() { rm -rf "$TMPDIR"; }
trap cleanup EXIT

SINK="$(pactl get-default-sink)"
DOT_NAME="$(printf '%s' "$NAME" | sed 's/\\/\\\\/g; s/"/\\"/g')"

echo "Measurement name:    $NAME"
echo "Output:              $SINK"
echo "Measurements:        $MEASUREMENTS per frequency and channel mode"
echo "Tone level:          $LEVEL"
echo "Normalization:       ${NORM_MIN}-${NORM_MAX} Hz, independently per mic/mode"
echo "Silence threshold:   ${SILENCE_THRESHOLD_DBFS} dBFS"
echo "Min response span:   ${MIN_RESPONSE_SPAN_DB} dB"
echo
echo "Candidate capture sources:"
for i in "${!SOURCES[@]}"; do
    printf '  Mic %-2d  %s\n' "$((i + 1))" "${SOURCES[$i]}"
done
echo

# Generate one mono tone, then route it into L, R, or both channels.
for freq in "${FREQUENCIES[@]}"; do
    mono="${TMPDIR}/tone-${freq}-mono.wav"
    sox -n -r "$RATE" -c 1 "$mono" synth "$DURATION" sine "$freq" vol "$LEVEL"
    sox "$mono" "${TMPDIR}/tone-${freq}-left.wav" remix 1 0
    sox "$mono" "${TMPDIR}/tone-${freq}-right.wav" remix 0 1
    sox "$mono" "${TMPDIR}/tone-${freq}-stereo.wav" remix 1 1
done

BASE_SCHEDULE="${TMPDIR}/schedule"
: > "$BASE_SCHEDULE"
for freq in "${FREQUENCIES[@]}"; do
    for ((i = 1; i <= MEASUREMENTS; i++)); do
        echo "$freq" >> "$BASE_SCHEDULE"
    done
done
shuf "$BASE_SCHEDULE" > "${BASE_SCHEDULE}.random"
mv "${BASE_SCHEDULE}.random" "$BASE_SCHEDULE"

TOTAL_JOBS=$(( ${#FREQUENCIES[@]} * MEASUREMENTS * ${#MODES[@]} ))
echo "Frequency repetition order:"
tr '\n' ' ' < "$BASE_SCHEDULE"
echo
echo "Within each repetition, left/right/stereo are shuffled independently."
echo

# RAW columns:
# 1 mic  2 source  3 mode  4 frequency  5 job_sequence  6 repetition  7 dbfs  8 valid
RAW="${TMPDIR}/raw.tsv"
: > "$RAW"
declare -A REPETITION

measure_job() {
    local freq="$1"
    local repetition="$2"
    local mode="$3"
    local job_sequence="$4"
    local tone="${TMPDIR}/tone-${freq}-${mode}.wav"

    declare -a PIDS=()
    declare -a RECORDINGS=()
    declare -a LOGS=()

    printf '[%3d/%3d] %5d Hz %-6s (%d/%d)' "$job_sequence" "$TOTAL_JOBS" "$freq" "$mode" "$repetition" "$MEASUREMENTS"

    for i in "${!SOURCES[@]}"; do
        local mic=$((i + 1))
        local source="${SOURCES[$i]}"
        local recording="${TMPDIR}/rec-${job_sequence}-${freq}-${mode}-m${mic}.wav"
        local logfile="${TMPDIR}/rec-${job_sequence}-${freq}-${mode}-m${mic}.log"
        RECORDINGS[$i]="$recording"
        LOGS[$i]="$logfile"

        pw-record --target="$source" --rate="$RATE" --channels=1 "$recording" >/dev/null 2>"$logfile" &
        PIDS[$i]=$!
    done

    sleep "$RECORD_LEAD"

    for i in "${!PIDS[@]}"; do
        if ! kill -0 "${PIDS[$i]}" 2>/dev/null; then
            echo
            echo "Recorder for Mic $((i + 1)) failed." >&2
            cat "${LOGS[$i]}" >&2 || true
            exit 1
        fi
    done

    pw-play --target="$SINK" "$tone" >/dev/null 2>&1
    sleep "$RECORD_TAIL"

    for pid in "${PIDS[@]}"; do kill -INT "$pid" 2>/dev/null || true; done
    for pid in "${PIDS[@]}"; do wait "$pid" 2>/dev/null || true; done

    local trim_start trim_duration
    trim_start="$(awk -v lead="$RECORD_LEAD" -v settle="$TONE_SETTLE" 'BEGIN { printf "%.6f", lead + settle }')"
    trim_duration="$(awk -v duration="$DURATION" -v settle="$TONE_SETTLE" 'BEGIN { printf "%.6f", duration - 2 * settle }')"

    for i in "${!SOURCES[@]}"; do
        local mic=$((i + 1))
        local source="${SOURCES[$i]}"
        local recording="${RECORDINGS[$i]}"
        local rms db valid

        rms="$(sox "$recording" -n trim "$trim_start" "$trim_duration" stat 2>&1 | awk '/RMS *amplitude/ { print $3 }')"
        if [[ -z "$rms" ]]; then
            db="-999"
            valid=0
        else
            db="$(awk -v x="$rms" 'BEGIN { if (x <= 0) print "-999"; else printf "%.3f", 20 * log(x) / log(10) }')"
            valid="$(awk -v db="$db" -v threshold="$SILENCE_THRESHOLD_DBFS" 'BEGIN { print (db >= threshold ? 1 : 0) }')"
        fi

        printf '%d\t%s\t%s\t%d\t%d\t%d\t%s\t%d\n' "$mic" "$source" "$mode" "$freq" "$job_sequence" "$repetition" "$db" "$valid" >> "$RAW"
        if (( valid )); then printf '  M%d:%7.2f' "$mic" "$db"; else printf '  M%d: silent' "$mic"; fi
    done
    echo
}

JOB_SEQUENCE=0
while read -r freq; do
    repetition="${REPETITION[$freq]:-0}"
    repetition=$((repetition + 1))
    REPETITION[$freq]="$repetition"
    mapfile -t MODE_ORDER < <(printf '%s\n' "${MODES[@]}" | shuf)
    for mode in "${MODE_ORDER[@]}"; do
        JOB_SEQUENCE=$((JOB_SEQUENCE + 1))
        measure_job "$freq" "$repetition" "$mode" "$JOB_SEQUENCE"
    done
done < "$BASE_SCHEDULE"

# SUMMARY columns:
# 1 mic  2 source  3 mode  4 freq  5 trimmed_mean  6 lowest  7 highest  8 valid_count
SUMMARY="${TMPDIR}/summary.tsv"
: > "$SUMMARY"
for i in "${!SOURCES[@]}"; do
    mic=$((i + 1))
    source="${SOURCES[$i]}"
    for mode in "${MODES[@]}"; do
        for freq in $(printf '%s\n' "${FREQUENCIES[@]}" | sort -n -u); do
            values="${TMPDIR}/values-m${mic}-${mode}-${freq}"
            awk -F '\t' -v m="$mic" -v mode="$mode" -v f="$freq" '$1 == m && $3 == mode && $4 == f && $8 == 1 { print $7 }' "$RAW" | sort -n > "$values"
            count="$(wc -l < "$values")"
            (( count >= 3 )) || continue
            lowest="$(head -n 1 "$values")"
            highest="$(tail -n 1 "$values")"
            trimmed_mean="$(sed '1d;$d' "$values" | awk '{ sum += $1; n++ } END { if (n) printf "%.3f", sum / n }')"
            printf '%d\t%s\t%s\t%d\t%s\t%s\t%s\t%d\n' "$mic" "$source" "$mode" "$freq" "$trimmed_mean" "$lowest" "$highest" "$count" >> "$SUMMARY"
        done
    done
done

# Reject silent or non-responsive pseudo-inputs.
ACTIVE="${TMPDIR}/active.tsv"
: > "$ACTIVE"
NUM_POINTS=$(( ${#FREQUENCIES[@]} * ${#MODES[@]} ))
MIN_ACTIVE_POINTS="$(awk -v n="$NUM_POINTS" -v fraction="$MIN_ACTIVE_FRACTION" 'BEGIN { x=n*fraction; printf "%d", (int(x)==x ? x : int(x)+1) }')"

echo
echo "Source validation:"
ACTIVE_COUNT=0
for i in "${!SOURCES[@]}"; do
    mic=$((i + 1))
    source="${SOURCES[$i]}"
    usable="$(awk -F '\t' -v m="$mic" '$1 == m { n++ } END { print n + 0 }' "$SUMMARY")"
    response_span="$(awk -F '\t' -v m="$mic" '$1 == m { if (!seen || $5 < min) min=$5; if (!seen || $5 > max) max=$5; seen=1 } END { if (!seen) print 0; else printf "%.3f", max-min }' "$SUMMARY")"
    enough_data="$(awk -v u="$usable" -v min="$MIN_ACTIVE_POINTS" 'BEGIN { print (u >= min ? 1 : 0) }')"
    responsive="$(awk -v s="$response_span" -v min="$MIN_RESPONSE_SPAN_DB" 'BEGIN { print (s >= min ? 1 : 0) }')"

    if (( enough_data && responsive )); then
        printf '%d\t%s\t%d\t%s\n' "$mic" "$source" "$usable" "$response_span" >> "$ACTIVE"
        ACTIVE_COUNT=$((ACTIVE_COUNT + 1))
        printf '  Mic %-2d ACTIVE    %2d/%d points   span=%6.2f dB  %s\n' "$mic" "$usable" "$NUM_POINTS" "$response_span" "$source"
    elif (( ! enough_data )); then
        printf '  Mic %-2d EXCLUDED  %2d/%d points   insufficient data  %s\n' "$mic" "$usable" "$NUM_POINTS" "$source"
    else
        printf '  Mic %-2d EXCLUDED  %2d/%d points   span=%6.2f dB < %.2f dB  %s\n' "$mic" "$usable" "$NUM_POINTS" "$response_span" "$MIN_RESPONSE_SPAN_DB" "$source"
    fi
done

if (( ACTIVE_COUNT == 0 )); then
    echo "No responsive microphones survived validation." >&2
    exit 1
fi

# NORMALIZED columns:
# 1 mic 2 source 3 mode 4 freq 5 mean_dbfs 6 normalized_db 7 lowest 8 highest 9 normalization_reference
NORMALIZED="${TMPDIR}/normalized.tsv"
: > "$NORMALIZED"
while IFS=$'\t' read -r mic source usable response_span; do
    for mode in "${MODES[@]}"; do
        ref="$(awk -F '\t' -v m="$mic" -v mode="$mode" -v lo="$NORM_MIN" -v hi="$NORM_MAX" '$1 == m && $3 == mode && $4 >= lo && $4 <= hi { sum += $5; n++ } END { if (!n) exit 1; printf "%.6f", sum/n }' "$SUMMARY")" || {
            echo "Mic $mic / $mode has no usable data in normalization band; skipping this mode." >&2
            continue
        }

        while IFS=$'\t' read -r m s md freq mean lowest highest count; do
            [[ "$m" == "$mic" && "$md" == "$mode" ]] || continue
            normalized="$(awk -v value="$mean" -v ref="$ref" 'BEGIN { printf "%.3f", value-ref }')"
            printf '%d\t%s\t%s\t%d\t%s\t%s\t%s\t%s\t%s\n' "$mic" "$source" "$mode" "$freq" "$mean" "$normalized" "$lowest" "$highest" "$ref" >> "$NORMALIZED"
        done < "$SUMMARY"
    done
done < "$ACTIVE"

# AVERAGE columns:
# 1 mode 2 frequency 3 normalized_average 4 between_mic_sd 5 contributing_mics
AVERAGE="${TMPDIR}/average.tsv"
: > "$AVERAGE"
for mode in "${MODES[@]}"; do
    for freq in $(printf '%s\n' "${FREQUENCIES[@]}" | sort -n -u); do
        awk -F '\t' -v mode="$mode" -v f="$freq" '$3 == mode && $4 == f { x=$6; sum+=x; sumsq+=x*x; n++ } END { if (!n) exit 1; mean=sum/n; variance=sumsq/n-mean*mean; if (variance<0) variance=0; printf "%s\t%d\t%.3f\t%.3f\t%d\n", mode, f, mean, sqrt(variance), n }' "$NORMALIZED" >> "$AVERAGE"
    done
done

echo
echo "Normalized averages across active microphones:"
printf '%10s  %10s  %10s  %10s\n' "Frequency" "Left" "Right" "Stereo"
for freq in $(printf '%s\n' "${FREQUENCIES[@]}" | sort -n -u); do
    left="$(awk -F '\t' -v f="$freq" '$1=="left" && $2==f {print $3}' "$AVERAGE")"
    right="$(awk -F '\t' -v f="$freq" '$1=="right" && $2==f {print $3}' "$AVERAGE")"
    stereo="$(awk -F '\t' -v f="$freq" '$1=="stereo" && $2==f {print $3}' "$AVERAGE")"
    printf '%8d Hz  %+8.2f  %+8.2f  %+8.2f\n' "$freq" "$left" "$right" "$stereo"
done

# CSV
{
    echo 'name,job_sequence,frequency_hz,repetition,mode,mic_id,source_name,rms_dbfs,valid_measurement,included_microphone,trimmed_mean_dbfs,normalized_db,normalized_mode_average_db,between_microphone_sd_db,contributing_mics,normalization_reference_dbfs'
    sort -t $'\t' -k5,5n -k1,1n "$RAW" |
    while IFS=$'\t' read -r mic source mode freq sequence repetition db valid; do
        included=0
        if awk -F '\t' -v m="$mic" '$1 == m {found=1} END {exit !found}' "$ACTIVE"; then included=1; fi
        mean=""; normalized=""; norm_ref=""
        if (( included )); then
            row="$(awk -F '\t' -v m="$mic" -v mode="$mode" -v f="$freq" '$1==m && $3==mode && $4==f {print $5 "\t" $6 "\t" $9}' "$NORMALIZED")"
            if [[ -n "$row" ]]; then
                mean="$(printf '%s\n' "$row" | cut -f1)"
                normalized="$(printf '%s\n' "$row" | cut -f2)"
                norm_ref="$(printf '%s\n' "$row" | cut -f3)"
            fi
        fi
        avg_row="$(awk -F '\t' -v mode="$mode" -v f="$freq" '$1==mode && $2==f {print $3 "\t" $4 "\t" $5}' "$AVERAGE")"
        avg="$(printf '%s\n' "$avg_row" | cut -f1)"
        sd="$(printf '%s\n' "$avg_row" | cut -f2)"
        contributors="$(printf '%s\n' "$avg_row" | cut -f3)"
        escaped_name="${NAME//\"/\"\"}"
        escaped_source="${source//\"/\"\"}"
        printf '"%s",%d,%d,%d,%s,%d,"%s",%s,%d,%d,%s,%s,%s,%s,%s,%s\n' "$escaped_name" "$sequence" "$freq" "$repetition" "$mode" "$mic" "$escaped_source" "$db" "$valid" "$included" "$mean" "$normalized" "$avg" "$sd" "$contributors" "$norm_ref"
    done
} > "$CSV"

MIN_FREQ="$(awk -F '\t' 'NR==1 || $2<min {min=$2} END {print min}' "$AVERAGE")"
MAX_FREQ="$(awk -F '\t' 'NR==1 || $2>max {max=$2} END {print max}' "$AVERAGE")"
MIN_DB="$(awk -F '\t' 'NR==1 || $6<min {min=$6} END {print min}' "$NORMALIZED")"
MAX_DB="$(awk -F '\t' 'NR==1 || $6>max {max=$6} END {print max}' "$NORMALIZED")"
Y_MIN="$(awk -v x="$MIN_DB" 'BEGIN { print int((x-5)/5)*5 }')"
Y_MAX="$(awk -v x="$MAX_DB" 'BEGIN { print int((x+10)/5)*5 }')"
(( Y_MAX > Y_MIN )) || Y_MAX=$((Y_MIN + 10))

graph_x() {
    awk -v f="$1" -v min="$MIN_FREQ" -v max="$MAX_FREQ" -v left="$X_LEFT" -v right="$X_RIGHT" 'BEGIN { if (max==min) printf "%.2f", left; else printf "%.2f", left+(right-left)*((log(f)-log(min))/(log(max)-log(min))) }'
}
graph_y() {
    awk -v db="$1" -v min="$Y_MIN" -v max="$Y_MAX" -v bottom="$Y_BOTTOM" -v top="$Y_TOP" 'BEGIN { printf "%.2f", bottom+(top-bottom)*((db-min)/(max-min)) }'
}
mode_color() {
    case "$1" in
        left) echo '#1f77b4' ;;
        right) echo '#d62728' ;;
        stereo) echo '#111111' ;;
    esac
}
mode_label() {
    case "$1" in
        left) echo 'Left' ;;
        right) echo 'Right' ;;
        stereo) echo 'Stereo' ;;
    esac
}

{
    cat <<EOF2
graph response {
    graph [layout=neato, overlap=false, splines=line, outputorder=edgesfirst, bgcolor="white", pad=0.55, label="${DOT_NAME} - L / R / stereo normalized response", labelloc=t, fontsize=20];
    node [shape=circle, fixedsize=true, width=0.07, height=0.07, label=""];
    edge [penwidth=1.0];
    canvas_bl [shape=point,width=0,pos="${X_LEFT},${Y_BOTTOM}!",style=invis];
    canvas_tr [shape=point,width=0,pos="${X_RIGHT},${Y_TOP}!",style=invis];
EOF2

    tick="$Y_MIN"
    while (( tick <= Y_MAX )); do
        y="$(graph_y "$tick")"
        id=$((tick - Y_MIN))
        printf 'ylabel_%d [shape=plaintext,pos="%d,%s!",label="%+d dB",fontsize=10];\n' "$id" "$((X_LEFT-60))" "$y" "$tick"
        printf 'gyl_%d [shape=point,width=0,pos="%d,%s!"];\n' "$id" "$X_LEFT" "$y"
        printf 'gyr_%d [shape=point,width=0,pos="%d,%s!"];\n' "$id" "$X_RIGHT" "$y"
        if (( tick == 0 )); then
            printf 'gyl_%d -- gyr_%d [color="#777777",style=dashed,penwidth=1.5];\n' "$id" "$id"
        else
            printf 'gyl_%d -- gyr_%d [color="#dddddd",style=dotted,penwidth=0.6];\n' "$id" "$id"
        fi
        tick=$((tick + 5))
    done

    for freq in 30 40 50 60 80 100 120 150 200 300 440 600 800; do
        (( freq >= MIN_FREQ && freq <= MAX_FREQ )) || continue
        x="$(graph_x "$freq")"
        printf 'xlabel_%d [shape=plaintext,pos="%s,%d!",label="%d",fontsize=9];\n' "$freq" "$x" "$((Y_BOTTOM-35))" "$freq"
        printf 'gxb_%d [shape=point,width=0,pos="%s,%d!"];\n' "$freq" "$x" "$Y_BOTTOM"
        printf 'gxt_%d [shape=point,width=0,pos="%s,%d!"];\n' "$freq" "$x" "$Y_TOP"
        printf 'gxb_%d -- gxt_%d [color="#eeeeee",style=dotted,penwidth=0.5];\n' "$freq" "$freq"
    done
    printf 'frequency_label [shape=plaintext,pos="%d,%d!",label="Frequency [Hz]",fontsize=11];\n' "$(((X_LEFT+X_RIGHT)/2))" "$((Y_BOTTOM-70))"

    # Thin dotted curves: each active mic, color-coded by playback mode.
    while IFS=$'\t' read -r mic source usable response_span; do
        for mode in "${MODES[@]}"; do
            color="$(mode_color "$mode")"
            previous=""
            while IFS=$'\t' read -r m s md freq mean normalized lowest highest ref; do
                x="$(graph_x "$freq")"; y="$(graph_y "$normalized")"; node="m${mic}_${mode}_f${freq}"
                printf '%s [pos="%s,%s!",color="%s",fillcolor="%s",style=filled,width=0.045,height=0.045];\n' "$node" "$x" "$y" "$color" "$color"
                if [[ -n "$previous" ]]; then printf '%s -- %s [color="%s",style=dotted,penwidth=0.7];\n' "$previous" "$node" "$color"; fi
                previous="$node"
            done < <(awk -F '\t' -v m="$mic" -v mode="$mode" '$1==m && $3==mode' "$NORMALIZED" | sort -t $'\t' -k4,4n)
        done
    done < "$ACTIVE"

    # Thick curves: normalized average over active microphones.
    for mode in "${MODES[@]}"; do
        color="$(mode_color "$mode")"
        previous=""
        while IFS=$'\t' read -r md freq average sd contributors; do
            x="$(graph_x "$freq")"; y="$(graph_y "$average")"; node="avg_${mode}_f${freq}"
            printf '%s [pos="%s,%s!",color="%s",fillcolor="%s",style=filled,width=0.11,height=0.11];\n' "$node" "$x" "$y" "$color" "$color"
            if [[ -n "$previous" ]]; then printf '%s -- %s [color="%s",penwidth=3.2];\n' "$previous" "$node" "$color"; fi
            previous="$node"
        done < <(awk -F '\t' -v mode="$mode" '$1==mode' "$AVERAGE" | sort -t $'\t' -k2,2n)
    done

    legend_x=$((X_RIGHT+145)); legend_y=$((Y_TOP-10))
    printf 'legend_title [shape=plaintext,pos="%d,%d!",label="Thick = mic average",fontsize=11];\n' "$legend_x" "$legend_y"
    legend_y=$((legend_y-35))
    for mode in "${MODES[@]}"; do
        color="$(mode_color "$mode")"; label="$(mode_label "$mode")"
        printf 'legend_%s [shape=plaintext,pos="%d,%d!",label="%s",fontcolor="%s",fontsize=11];\n' "$mode" "$legend_x" "$legend_y" "$label" "$color"
        legend_y=$((legend_y-30))
    done
    printf 'legend_note [shape=plaintext,pos="%d,%d!",label="Thin dotted = individual microphones",fontsize=9];\n' "$legend_x" "$((legend_y-10))"
    echo '}'
} > "$DOT"

neato -n2 -Tpng -Gdpi=150 "$DOT" -o "$PNG"

echo
echo "Finished."
echo "CSV: $CSV"
echo "PNG: $PNG"
echo "DOT: $DOT"
