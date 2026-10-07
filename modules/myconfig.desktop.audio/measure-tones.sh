#!/usr/bin/env nix-shell
#! nix-shell -i bash -p sox pipewire pulseaudio coreutils gawk graphviz gnused

set -euo pipefail

# ---------------------------------------------------------------------------
# Defaults
# ---------------------------------------------------------------------------

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

OUTDIR="/tmp/measure-tones"

RECORD_LEAD=0.30
TONE_SETTLE=0.25
RECORD_TAIL=0.15

NORM_MIN=100
NORM_MAX=300

# Measurements below this are treated as silence / unusable.
SILENCE_THRESHOLD_DBFS=-100

# A source must yield a usable trimmed mean for at least this fraction
# of all tested frequencies or it is excluded entirely.
MIN_ACTIVE_FRACTION=0.50

SOURCE_MODE="all"
CUSTOM_SOURCES=""

X_LEFT=120
X_RIGHT=1120
Y_BOTTOM=100
Y_TOP=700

# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

usage() {
    cat <<EOF
Usage: $0 [options]

Options:
  --name NAME
      Measurement name.
      Default: measurement

  --measurements N
      Measurements per frequency.
      Default: 5

  --duration SECONDS
      Tone duration.
      Default: 2.0

  --level AMPLITUDE
      Digital tone amplitude.
      Default: 0.1

  --frequencies "30 40 50 ..."
      Override frequency list.

  --normalize MIN MAX
      Normalize every microphone independently using this frequency range.
      Default: 100 300

  --silence-threshold DBFS
      Measurements below this level are considered invalid/silent.
      Default: -100

  --sources all
      Use all non-monitor capture sources.
      Default.

  --sources default
      Use only the current default source.

  --sources "SOURCE1,SOURCE2,..."
      Explicit source list.

  --list-sources
      List capture sources and exit.

  --help

Example:
  $0 --name kali
EOF
}

LIST_SOURCES=0

while [[ $# -gt 0 ]]; do
    case "$1" in
        --name)
            NAME="$2"
            shift 2
            ;;
        --measurements)
            MEASUREMENTS="$2"
            shift 2
            ;;
        --duration)
            DURATION="$2"
            shift 2
            ;;
        --level)
            LEVEL="$2"
            shift 2
            ;;
        --frequencies)
            read -r -a FREQUENCIES <<< "$2"
            shift 2
            ;;
        --normalize)
            NORM_MIN="$2"
            NORM_MAX="$3"
            shift 3
            ;;
        --silence-threshold)
            SILENCE_THRESHOLD_DBFS="$2"
            shift 2
            ;;
        --sources)
            case "$2" in
                all)
                    SOURCE_MODE="all"
                    CUSTOM_SOURCES=""
                    ;;
                default)
                    SOURCE_MODE="default"
                    CUSTOM_SOURCES=""
                    ;;
                *)
                    SOURCE_MODE="custom"
                    CUSTOM_SOURCES="$2"
                    ;;
            esac
            shift 2
            ;;
        --list-sources)
            LIST_SOURCES=1
            shift
            ;;
        --help|-h)
            usage
            exit 0
            ;;
        *)
            echo "Unknown argument: $1" >&2
            usage >&2
            exit 1
            ;;
    esac
done

if (( MEASUREMENTS < 3 )); then
    echo "--measurements must be >= 3" >&2
    exit 1
fi

mkdir -p "$OUTDIR"

# ---------------------------------------------------------------------------
# Source discovery
# ---------------------------------------------------------------------------

DEFAULT_SOURCE="$(pactl get-default-source)"

mapfile -t AVAILABLE_SOURCES < <(
    pactl list short sources |
        awk -F '\t' '$2 !~ /\.monitor$/ { print $2 }'
)

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
    default)
        SOURCES=("$DEFAULT_SOURCE")
        ;;

    all)
        # Default first.
        SOURCES+=("$DEFAULT_SOURCE")

        for source in "${AVAILABLE_SOURCES[@]}"; do
            [[ "$source" == "$DEFAULT_SOURCE" ]] && continue
            SOURCES+=("$source")
        done
        ;;

    custom)
        IFS=',' read -r -a SOURCES <<< "$CUSTOM_SOURCES"
        ;;
esac

# Remove duplicates.
declare -A SEEN_SOURCES
declare -a UNIQUE_SOURCES=()

for source in "${SOURCES[@]}"; do
    [[ -n "${SEEN_SOURCES[$source]:-}" ]] && continue
    SEEN_SOURCES[$source]=1
    UNIQUE_SOURCES+=("$source")
done

SOURCES=("${UNIQUE_SOURCES[@]}")

if [[ ${#SOURCES[@]} -eq 0 ]]; then
    echo "No sources found." >&2
    exit 1
fi

# ---------------------------------------------------------------------------
# Output paths
# ---------------------------------------------------------------------------

SAFE_NAME="$(
    printf '%s' "$NAME" |
        tr '[:space:]' '-' |
        tr -cd '[:alnum:]_.-'
)"

[[ -n "$SAFE_NAME" ]] || SAFE_NAME="measurement"

TIMESTAMP="$(date +%Y%m%d-%H%M%S)"
PREFIX="${SAFE_NAME}-${TIMESTAMP}"

CSV="${OUTDIR}/${PREFIX}.csv"
PNG="${OUTDIR}/${PREFIX}.png"
DOT="${OUTDIR}/${PREFIX}.dot"

TMPDIR="$(mktemp -d "${OUTDIR}/tmp.XXXXXX")"

cleanup() {
    rm -rf "$TMPDIR"
}

trap cleanup EXIT

SINK="$(pactl get-default-sink)"

echo "Measurement name:   $NAME"
echo "Output:             $SINK"
echo "Measurements:       $MEASUREMENTS per frequency"
echo "Tone level:         $LEVEL"
echo "Normalization:      ${NORM_MIN}-${NORM_MAX} Hz"
echo "Silence threshold:  ${SILENCE_THRESHOLD_DBFS} dBFS"
echo

echo "Candidate capture sources:"

for i in "${!SOURCES[@]}"; do
    printf '  Mic %-2d  %s\n' "$((i + 1))" "${SOURCES[$i]}"
done

echo

# ---------------------------------------------------------------------------
# Tone files
# ---------------------------------------------------------------------------

for freq in "${FREQUENCIES[@]}"; do
    sox \
        -n \
        -r "$RATE" \
        -c 2 \
        "${TMPDIR}/tone-${freq}.wav" \
        synth "$DURATION" sine "$freq" \
        vol "$LEVEL"
done

# ---------------------------------------------------------------------------
# Random schedule
# ---------------------------------------------------------------------------

SCHEDULE="${TMPDIR}/schedule"
: > "$SCHEDULE"

for freq in "${FREQUENCIES[@]}"; do
    for ((i = 1; i <= MEASUREMENTS; i++)); do
        echo "$freq" >> "$SCHEDULE"
    done
done

shuf "$SCHEDULE" > "${SCHEDULE}.random"
mv "${SCHEDULE}.random" "$SCHEDULE"

TOTAL=$(( ${#FREQUENCIES[@]} * MEASUREMENTS ))

echo "Randomized test sequence:"
tr '\n' ' ' < "$SCHEDULE"
echo
echo

# ---------------------------------------------------------------------------
# RAW columns
#
# 1 mic
# 2 source
# 3 frequency
# 4 sequence
# 5 repetition
# 6 dBFS
# 7 valid (0/1)
# ---------------------------------------------------------------------------

RAW="${TMPDIR}/raw.tsv"
: > "$RAW"

declare -A REPETITION

measure_frequency() {
    local freq="$1"
    local sequence="$2"

    local repetition="${REPETITION[$freq]:-0}"
    repetition=$((repetition + 1))
    REPETITION[$freq]="$repetition"

    local tone="${TMPDIR}/tone-${freq}.wav"

    declare -a PIDS=()
    declare -a RECORDINGS=()
    declare -a LOGS=()

    printf \
        "[%3d/%3d] %5d Hz (%d/%d)" \
        "$sequence" \
        "$TOTAL" \
        "$freq" \
        "$repetition" \
        "$MEASUREMENTS"

    for i in "${!SOURCES[@]}"; do
        mic=$((i + 1))
        source="${SOURCES[$i]}"

        recording="${TMPDIR}/rec-${sequence}-${freq}-m${mic}.wav"
        logfile="${TMPDIR}/rec-${sequence}-${freq}-m${mic}.log"

        RECORDINGS[$i]="$recording"
        LOGS[$i]="$logfile"

        pw-record \
            --target="$source" \
            --rate="$RATE" \
            --channels=1 \
            "$recording" \
            >/dev/null 2>"$logfile" &

        PIDS[$i]=$!
    done

    sleep "$RECORD_LEAD"

    pw-play \
        --target="$SINK" \
        "$tone" \
        >/dev/null 2>&1

    sleep "$RECORD_TAIL"

    for pid in "${PIDS[@]}"; do
        kill -INT "$pid" 2>/dev/null || true
    done

    for pid in "${PIDS[@]}"; do
        wait "$pid" 2>/dev/null || true
    done

    trim_start="$(
        awk \
            -v lead="$RECORD_LEAD" \
            -v settle="$TONE_SETTLE" \
            'BEGIN { printf "%.6f", lead + settle }'
    )"

    trim_duration="$(
        awk \
            -v duration="$DURATION" \
            -v settle="$TONE_SETTLE" \
            'BEGIN { printf "%.6f", duration - (2 * settle) }'
    )"

    for i in "${!SOURCES[@]}"; do
        mic=$((i + 1))
        source="${SOURCES[$i]}"
        recording="${RECORDINGS[$i]}"

        rms="$(
            sox \
                "$recording" \
                -n \
                trim "$trim_start" "$trim_duration" \
                stat 2>&1 |
            awk '/RMS *amplitude/ { print $3 }'
        )"

        if [[ -z "$rms" ]]; then
            db="-999"
            valid=0
        else
            db="$(
                awk -v x="$rms" '
                    BEGIN {
                        if (x <= 0)
                            print "-999"
                        else
                            printf "%.3f", 20 * log(x) / log(10)
                    }
                '
            )"

            valid="$(
                awk \
                    -v db="$db" \
                    -v threshold="$SILENCE_THRESHOLD_DBFS" '
                    BEGIN {
                        print (db >= threshold ? 1 : 0)
                    }
                '
            )"
        fi

        printf \
            '%d\t%s\t%d\t%d\t%d\t%s\t%d\n' \
            "$mic" \
            "$source" \
            "$freq" \
            "$sequence" \
            "$repetition" \
            "$db" \
            "$valid" \
            >> "$RAW"

        if (( valid )); then
            printf "  M%d:%7.2f" "$mic" "$db"
        else
            printf "  M%d: silent" "$mic"
        fi
    done

    echo
}

SEQUENCE=0

while read -r freq; do
    SEQUENCE=$((SEQUENCE + 1))
    measure_frequency "$freq" "$SEQUENCE"
done < "$SCHEDULE"

# ---------------------------------------------------------------------------
# Per-mic/per-frequency trimmed means.
#
# Only valid measurements participate.
#
# SUMMARY:
# 1 mic
# 2 source
# 3 frequency
# 4 trimmed_mean
# 5 lowest
# 6 highest
# 7 valid_measurement_count
# ---------------------------------------------------------------------------

SUMMARY="${TMPDIR}/summary.tsv"
: > "$SUMMARY"

for i in "${!SOURCES[@]}"; do
    mic=$((i + 1))
    source="${SOURCES[$i]}"

    for freq in $(printf '%s\n' "${FREQUENCIES[@]}" | sort -n -u); do

        values="${TMPDIR}/values-m${mic}-${freq}"

        awk \
            -F '\t' \
            -v m="$mic" \
            -v f="$freq" \
            '$1 == m && $3 == f && $7 == 1 { print $6 }' \
            "$RAW" |
            sort -n > "$values"

        count="$(wc -l < "$values")"

        # Need at least three valid measurements to remove low/high.
        if (( count < 3 )); then
            continue
        fi

        lowest="$(head -n 1 "$values")"
        highest="$(tail -n 1 "$values")"

        trimmed_mean="$(
            sed '1d;$d' "$values" |
                awk '
                    {
                        sum += $1
                        n++
                    }
                    END {
                        if (n > 0)
                            printf "%.3f", sum / n
                    }
                '
        )"

        printf \
            '%d\t%s\t%d\t%s\t%s\t%s\t%d\n' \
            "$mic" \
            "$source" \
            "$freq" \
            "$trimmed_mean" \
            "$lowest" \
            "$highest" \
            "$count" \
            >> "$SUMMARY"
    done
done

# ---------------------------------------------------------------------------
# Determine active microphones.
#
# A source must have usable summary data for at least 50% of frequencies.
# ---------------------------------------------------------------------------

ACTIVE="${TMPDIR}/active.tsv"
: > "$ACTIVE"

NUM_FREQS="${#FREQUENCIES[@]}"

MIN_ACTIVE_FREQS="$(
    awk \
        -v n="$NUM_FREQS" \
        -v f="$MIN_ACTIVE_FRACTION" '
        BEGIN {
            x = n * f
            print int(x) == x ? int(x) : int(x) + 1
        }
    '
)"

echo
echo "Source validation:"

ACTIVE_COUNT=0

for i in "${!SOURCES[@]}"; do
    mic=$((i + 1))
    source="${SOURCES[$i]}"

    usable="$(
        awk \
            -F '\t' \
            -v m="$mic" \
            '$1 == m { n++ } END { print n + 0 }' \
            "$SUMMARY"
    )"

    if (( usable >= MIN_ACTIVE_FREQS )); then
        echo -e "${mic}\t${source}" >> "$ACTIVE"
        ACTIVE_COUNT=$((ACTIVE_COUNT + 1))

        printf \
            "  Mic %-2d ACTIVE    %2d/%d frequencies  %s\n" \
            "$mic" \
            "$usable" \
            "$NUM_FREQS" \
            "$source"
    else
        printf \
            "  Mic %-2d EXCLUDED  %2d/%d frequencies  %s\n" \
            "$mic" \
            "$usable" \
            "$NUM_FREQS" \
            "$source"
    fi
done

if (( ACTIVE_COUNT == 0 )); then
    echo "No active microphones survived validation." >&2
    exit 1
fi

# ---------------------------------------------------------------------------
# Normalize each active mic independently.
#
# NORMALIZED:
# 1 mic
# 2 source
# 3 freq
# 4 mean_dbfs
# 5 normalized_db
# 6 lowest
# 7 highest
# 8 normalization_reference
# ---------------------------------------------------------------------------

NORMALIZED="${TMPDIR}/normalized.tsv"
: > "$NORMALIZED"

while IFS=$'\t' read -r mic source; do

    ref="$(
        awk \
            -F '\t' \
            -v m="$mic" \
            -v lo="$NORM_MIN" \
            -v hi="$NORM_MAX" '
            $1 == m && $3 >= lo && $3 <= hi {
                sum += $4
                n++
            }
            END {
                if (n == 0)
                    exit 1
                printf "%.6f", sum / n
            }
        ' "$SUMMARY"
    )" || {
        echo \
            "Mic $mic has no usable data in normalization band; excluding it." \
            >&2
        continue
    }

    while IFS=$'\t' read -r m s freq mean lowest highest count; do
        [[ "$m" == "$mic" ]] || continue

        normalized="$(
            awk \
                -v value="$mean" \
                -v ref="$ref" \
                'BEGIN { printf "%.3f", value - ref }'
        )"

        printf \
            '%d\t%s\t%d\t%s\t%s\t%s\t%s\t%s\n' \
            "$mic" \
            "$source" \
            "$freq" \
            "$mean" \
            "$normalized" \
            "$lowest" \
            "$highest" \
            "$ref" \
            >> "$NORMALIZED"

    done < "$SUMMARY"

done < "$ACTIVE"

# ---------------------------------------------------------------------------
# Normalized average.
#
# Only available, validated microphone values at a frequency are averaged.
#
# AVERAGE:
# 1 frequency
# 2 average_normalized_db
# 3 between_mic_sd_db
# 4 contributing_mics
# ---------------------------------------------------------------------------

AVERAGE="${TMPDIR}/average.tsv"
: > "$AVERAGE"

for freq in $(printf '%s\n' "${FREQUENCIES[@]}" | sort -n -u); do

    awk \
        -F '\t' \
        -v f="$freq" '
        $3 == f {
            x = $5
            sum += x
            sumsq += x * x
            n++
        }

        END {
            if (n == 0)
                exit 1

            mean = sum / n
            variance = sumsq / n - mean * mean

            if (variance < 0)
                variance = 0

            printf "%d\t%.3f\t%.3f\t%d\n",
                   f, mean, sqrt(variance), n
        }
    ' "$NORMALIZED" >> "$AVERAGE"

done

# ---------------------------------------------------------------------------
# Console result
# ---------------------------------------------------------------------------

echo
printf "%10s  %10s  %10s  %5s\n" \
    "Frequency" "Average" "Mic SD" "Mics"

while IFS=$'\t' read -r freq avg sd count; do
    printf \
        "%8d Hz  %+8.2f dB  %7.2f dB  %5d\n" \
        "$freq" \
        "$avg" \
        "$sd" \
        "$count"
done < "$AVERAGE"

# ---------------------------------------------------------------------------
# CSV
# ---------------------------------------------------------------------------

{
    echo "name,sequence,frequency_hz,repetition,mic_id,source_name,rms_dbfs,valid_measurement,included_microphone,trimmed_mean_dbfs,normalized_db,normalized_average_db,between_microphone_sd_db,contributing_mics,normalization_reference_dbfs"

    sort -t $'\t' -k4,4n -k1,1n "$RAW" |
    while IFS=$'\t' read -r mic source freq sequence repetition db valid; do

        if awk \
            -F '\t' \
            -v m="$mic" \
            '$1 == m { found=1 } END { exit !found }' \
            "$ACTIVE"
        then
            included=1
        else
            included=0
        fi

        mean=""
        normalized=""
        norm_ref=""

        if (( included )); then
            row="$(
                awk \
                    -F '\t' \
                    -v m="$mic" \
                    -v f="$freq" \
                    '$1 == m && $3 == f {
                        print $4 "\t" $5 "\t" $8
                    }' \
                    "$NORMALIZED"
            )"

            if [[ -n "$row" ]]; then
                mean="$(printf '%s\n' "$row" | cut -f1)"
                normalized="$(printf '%s\n' "$row" | cut -f2)"
                norm_ref="$(printf '%s\n' "$row" | cut -f3)"
            fi
        fi

        avg_row="$(
            awk \
                -F '\t' \
                -v f="$freq" \
                '$1 == f { print $2 "\t" $3 "\t" $4 }' \
                "$AVERAGE"
        )"

        avg="$(printf '%s\n' "$avg_row" | cut -f1)"
        sd="$(printf '%s\n' "$avg_row" | cut -f2)"
        contributors="$(printf '%s\n' "$avg_row" | cut -f3)"

        escaped_name="${NAME//\"/\"\"}"
        escaped_source="${source//\"/\"\"}"

        printf \
            '"%s",%d,%d,%d,%d,"%s",%s,%d,%d,%s,%s,%s,%s,%s,%s\n' \
            "$escaped_name" \
            "$sequence" \
            "$freq" \
            "$repetition" \
            "$mic" \
            "$escaped_source" \
            "$db" \
            "$valid" \
            "$included" \
            "$mean" \
            "$normalized" \
            "$avg" \
            "$sd" \
            "$contributors" \
            "$norm_ref"
    done
} > "$CSV"

# ---------------------------------------------------------------------------
# Plot ranges
# ---------------------------------------------------------------------------

MIN_FREQ="$(
    awk -F '\t' 'NR==1 || $1<min {min=$1} END {print min}' "$AVERAGE"
)"

MAX_FREQ="$(
    awk -F '\t' 'NR==1 || $1>max {max=$1} END {print max}' "$AVERAGE"
)"

MIN_DB="$(
    awk -F '\t' 'NR==1 || $5<min {min=$5} END {print min}' "$NORMALIZED"
)"

MAX_DB="$(
    awk -F '\t' 'NR==1 || $5>max {max=$5} END {print max}' "$NORMALIZED"
)"

Y_MIN="$(
    awk -v x="$MIN_DB" '
        BEGIN { print int((x - 5) / 5) * 5 }
    '
)"

Y_MAX="$(
    awk -v x="$MAX_DB" '
        BEGIN { print int((x + 10) / 5) * 5 }
    '
)"

(( Y_MAX > Y_MIN )) || Y_MAX=$((Y_MIN + 10))

graph_x() {
    awk \
        -v f="$1" \
        -v min="$MIN_FREQ" \
        -v max="$MAX_FREQ" \
        -v left="$X_LEFT" \
        -v right="$X_RIGHT" '
        BEGIN {
            printf "%.2f",
                left + (right-left) *
                ((log(f)-log(min)) / (log(max)-log(min)))
        }
    '
}

graph_y() {
    awk \
        -v db="$1" \
        -v min="$Y_MIN" \
        -v max="$Y_MAX" \
        -v bottom="$Y_BOTTOM" \
        -v top="$Y_TOP" '
        BEGIN {
            printf "%.2f",
                bottom + (top-bottom) *
                ((db-min)/(max-min))
        }
    '
}

COLORS=(
    "#1f77b4"
    "#d62728"
    "#2ca02c"
    "#9467bd"
    "#ff7f0e"
    "#17becf"
    "#8c564b"
    "#e377c2"
)

# ---------------------------------------------------------------------------
# Graphviz
# ---------------------------------------------------------------------------

{
    cat <<EOF
graph response {
    graph [
        layout=neato,
        overlap=false,
        splines=line,
        outputorder=edgesfirst,
        bgcolor="white",
        pad=0.55,
        label="${NAME} - normalized multi-microphone response",
        labelloc=t,
        fontsize=20
    ];

    node [
        shape=circle,
        fixedsize=true,
        width=0.08,
        height=0.08,
        label=""
    ];

    edge [penwidth=1.5];

    canvas_bl [
        shape=point,
        width=0,
        pos="${X_LEFT},${Y_BOTTOM}!",
        style=invis
    ];

    canvas_tr [
        shape=point,
        width=0,
        pos="${X_RIGHT},${Y_TOP}!",
        style=invis
    ];
EOF

    # Y grid.
    tick="$Y_MIN"

    while (( tick <= Y_MAX )); do
        y="$(graph_y "$tick")"
        id=$((tick - Y_MIN))

        printf \
            'ylabel_%d [shape=plaintext,pos="%d,%s!",label="%+d dB",fontsize=10];\n' \
            "$id" "$((X_LEFT - 60))" "$y" "$tick"

        printf \
            'gyl_%d [shape=point,width=0,pos="%d,%s!"];\n' \
            "$id" "$X_LEFT" "$y"

        printf \
            'gyr_%d [shape=point,width=0,pos="%d,%s!"];\n' \
            "$id" "$X_RIGHT" "$y"

        if (( tick == 0 )); then
            printf \
                'gyl_%d -- gyr_%d [color="#777777",style=dashed,penwidth=1.5];\n' \
                "$id" "$id"
        else
            printf \
                'gyl_%d -- gyr_%d [color="#dddddd",style=dotted,penwidth=0.6];\n' \
                "$id" "$id"
        fi

        tick=$((tick + 5))
    done

    # X labels.
    for freq in 30 40 50 60 80 100 120 150 200 300 440 600 800; do
        (( freq >= MIN_FREQ && freq <= MAX_FREQ )) || continue

        x="$(graph_x "$freq")"

        printf \
            'xlabel_%d [shape=plaintext,pos="%s,%d!",label="%d",fontsize=9];\n' \
            "$freq" "$x" "$((Y_BOTTOM - 35))" "$freq"
    done

    printf \
        'frequency_label [shape=plaintext,pos="%d,%d!",label="Frequency [Hz]",fontsize=11];\n' \
        "$(((X_LEFT + X_RIGHT) / 2))" \
        "$((Y_BOTTOM - 70))"

    # ---------------------------------------------------------------
    # Active microphone curves only
    # ---------------------------------------------------------------

    curve_index=0

    while IFS=$'\t' read -r mic source; do
        color="${COLORS[$((curve_index % ${#COLORS[@]}))]}"
        previous=""

        while IFS=$'\t' read -r m s freq mean normalized lowest highest ref; do
            [[ "$m" == "$mic" ]] || continue

            x="$(graph_x "$freq")"
            y="$(graph_y "$normalized")"

            node="m${mic}_f${freq}"

            printf \
                '%s [pos="%s,%s!",color="%s",fillcolor="%s",style=filled];\n' \
                "$node" "$x" "$y" "$color" "$color"

            if [[ -n "$previous" ]]; then
                printf \
                    '%s -- %s [color="%s",penwidth=1.5];\n' \
                    "$previous" "$node" "$color"
            fi

            previous="$node"

        done < "$NORMALIZED"

        curve_index=$((curve_index + 1))

    done < "$ACTIVE"

    # Average.
    previous=""

    while IFS=$'\t' read -r freq average sd contributors; do
        x="$(graph_x "$freq")"
        y="$(graph_y "$average")"

        node="avg_f${freq}"

        printf \
            '%s [pos="%s,%s!",color="black",fillcolor="black",style=filled,width=0.11,height=0.11];\n' \
            "$node" "$x" "$y"

        if [[ -n "$previous" ]]; then
            printf \
                '%s -- %s [color="black",penwidth=3.2];\n' \
                "$previous" "$node"
        fi

        previous="$node"

    done < "$AVERAGE"

    # Legend.
    legend_x=$((X_RIGHT + 140))
    legend_y=$((Y_TOP - 20))

    printf \
        'legend_title [shape=plaintext,pos="%d,%d!",label="Curves",fontsize=11];\n' \
        "$legend_x" "$legend_y"

    legend_y=$((legend_y - 35))
    curve_index=0

    while IFS=$'\t' read -r mic source; do
        color="${COLORS[$((curve_index % ${#COLORS[@]}))]}"

        printf \
            'legend_m%d [shape=plaintext,pos="%d,%d!",label="Mic %d",fontcolor="%s",fontsize=10];\n' \
            "$mic" "$legend_x" "$legend_y" "$mic" "$color"

        legend_y=$((legend_y - 28))
        curve_index=$((curve_index + 1))
    done < "$ACTIVE"

    printf \
        'legend_avg [shape=plaintext,pos="%d,%d!",label="Normalized average",fontcolor="black",fontsize=10];\n' \
        "$legend_x" "$legend_y"

    echo "}"

} > "$DOT"

neato \
    -n2 \
    -Tpng \
    -Gdpi=150 \
    "$DOT" \
    -o "$PNG"

echo
echo "Finished."
echo "CSV: $CSV"
echo "PNG: $PNG"
echo "DOT: $DOT"
