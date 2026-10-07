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

# "all"     = all non-monitor PipeWire/PulseAudio sources
# "default" = only current default source
SOURCE_MODE="all"
CUSTOM_SOURCES=""

# Graph dimensions, in Graphviz points.
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

      Lowest and highest measurement per microphone/frequency are
      discarded; the remaining measurements are averaged.

  --duration SECONDS
      Duration of each tone.
      Default: 2.0

  --level AMPLITUDE
      Digital tone amplitude.
      Default: 0.1 (~ -20 dBFS)

  --frequencies "30 40 50 ..."
      Override frequency list.

  --normalize MIN MAX
      Normalize every microphone independently against its average
      response between MIN and MAX Hz.
      Default: 100 300

  --sources all
      Use all non-monitor capture sources.
      This is the default.

  --sources default
      Use only the current default source.

  --sources "SOURCE1,SOURCE2,..."
      Explicitly select PipeWire/PulseAudio source names.

  --list-sources
      Print available non-monitor sources and exit.

  --help
      Show this help.

Examples:

  $0 --name kali

  $0 --name kali --sources default

  $0 --name kali --measurements 7

  $0 --list-sources

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

# ---------------------------------------------------------------------------
# Validation
# ---------------------------------------------------------------------------

if (( MEASUREMENTS < 3 )); then
    echo "--measurements must be at least 3" >&2
    exit 1
fi

if [[ ${#FREQUENCIES[@]} -lt 1 ]]; then
    echo "At least one frequency is required" >&2
    exit 1
fi

mkdir -p "$OUTDIR"

# ---------------------------------------------------------------------------
# Discover sources
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
            printf '  * %s  [default]\n' "$source"
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
        # Put the default microphone first.
        found_default=0

        for source in "${AVAILABLE_SOURCES[@]}"; do
            if [[ "$source" == "$DEFAULT_SOURCE" ]]; then
                SOURCES+=("$source")
                found_default=1
                break
            fi
        done

        if (( ! found_default )); then
            echo "Default source was not found among available capture sources:" >&2
            echo "  $DEFAULT_SOURCE" >&2
            exit 1
        fi

        for source in "${AVAILABLE_SOURCES[@]}"; do
            if [[ "$source" != "$DEFAULT_SOURCE" ]]; then
                SOURCES+=("$source")
            fi
        done
        ;;

    custom)
        IFS=',' read -r -a SOURCES <<< "$CUSTOM_SOURCES"
        ;;
esac

if [[ ${#SOURCES[@]} -lt 1 ]]; then
    echo "No capture sources found." >&2
    exit 1
fi

# Validate explicitly selected sources.
for source in "${SOURCES[@]}"; do
    found=0

    for available in "${AVAILABLE_SOURCES[@]}"; do
        if [[ "$source" == "$available" ]]; then
            found=1
            break
        fi
    done

    if (( ! found )); then
        echo "Capture source does not exist:" >&2
        echo "  $source" >&2
        echo >&2
        echo "Run with --list-sources to see available sources." >&2
        exit 1
    fi
done

# ---------------------------------------------------------------------------
# Output files
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

# ---------------------------------------------------------------------------
# Configuration summary
# ---------------------------------------------------------------------------

echo "Measurement name: $NAME"
echo "Output:           $SINK"
echo "Measurements:     $MEASUREMENTS per frequency"
echo "Tone duration:    ${DURATION}s"
echo "Tone level:       $LEVEL"
echo "Normalization:    ${NORM_MIN}-${NORM_MAX} Hz"
echo

echo "Microphones / capture sources:"

for i in "${!SOURCES[@]}"; do
    mic=$((i + 1))
    source="${SOURCES[$i]}"

    if [[ "$source" == "$DEFAULT_SOURCE" ]]; then
        printf "  Mic %-2d  %s  [default]\n" "$mic" "$source"
    else
        printf "  Mic %-2d  %s\n" "$mic" "$source"
    fi
done

echo
echo "Frequencies:"
printf '  %s\n' "${FREQUENCIES[*]}"
echo

# ---------------------------------------------------------------------------
# Generate stereo tone WAVs
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
# Randomized test schedule
# ---------------------------------------------------------------------------

SCHEDULE="${TMPDIR}/schedule"
: > "$SCHEDULE"

for freq in "${FREQUENCIES[@]}"; do
    for ((i = 1; i <= MEASUREMENTS; i++)); do
        printf '%s\n' "$freq" >> "$SCHEDULE"
    done
done

shuf "$SCHEDULE" > "${SCHEDULE}.shuffled"
mv "${SCHEDULE}.shuffled" "$SCHEDULE"

TOTAL=$(( ${#FREQUENCIES[@]} * MEASUREMENTS ))

echo "Randomized test sequence:"
tr '\n' ' ' < "$SCHEDULE"
echo
echo

# ---------------------------------------------------------------------------
# Raw data
#
# Columns:
#
# 1 mic_id
# 2 source_name
# 3 frequency
# 4 sequence
# 5 repetition
# 6 rms_dbfs
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

    # ---------------------------------------------------------------
    # Start every microphone before playing the tone.
    # ---------------------------------------------------------------

    for i in "${!SOURCES[@]}"; do
        mic=$((i + 1))
        source="${SOURCES[$i]}"

        recording="${TMPDIR}/recording-${sequence}-${freq}-mic${mic}.wav"
        logfile="${TMPDIR}/recording-${sequence}-${freq}-mic${mic}.log"

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

    # Make sure all recorders survived startup.
    for i in "${!PIDS[@]}"; do
        if ! kill -0 "${PIDS[$i]}" 2>/dev/null; then
            echo
            echo "Recorder for Mic $((i + 1)) failed to start." >&2
            cat "${LOGS[$i]}" >&2 || true
            exit 1
        fi
    done

    # ---------------------------------------------------------------
    # One tone, simultaneously captured by every microphone.
    # ---------------------------------------------------------------

    pw-play \
        --target="$SINK" \
        "$tone" \
        >/dev/null 2>&1

    sleep "$RECORD_TAIL"

    # ---------------------------------------------------------------
    # Stop all recorders.
    # ---------------------------------------------------------------

    for pid in "${PIDS[@]}"; do
        kill -INT "$pid" 2>/dev/null || true
    done

    for pid in "${PIDS[@]}"; do
        wait "$pid" 2>/dev/null || true
    done

    local trim_start
    local trim_duration

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

    # ---------------------------------------------------------------
    # Analyze each microphone independently.
    # ---------------------------------------------------------------

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
            echo
            echo "Could not determine RMS for Mic $mic" >&2
            exit 1
        fi

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

        printf \
            '%d\t%s\t%d\t%d\t%d\t%s\n' \
            "$mic" \
            "$source" \
            "$freq" \
            "$sequence" \
            "$repetition" \
            "$db" \
            >> "$RAW"

        printf "  M%d:%7.2f" "$mic" "$db"
    done

    echo
}

SEQUENCE=0

while read -r freq; do
    SEQUENCE=$((SEQUENCE + 1))
    measure_frequency "$freq" "$SEQUENCE"
done < "$SCHEDULE"

# ---------------------------------------------------------------------------
# Trimmed mean per microphone + frequency
#
# Columns:
#
# 1 mic_id
# 2 source_name
# 3 frequency
# 4 trimmed_mean_dbfs
# 5 lowest_dbfs
# 6 highest_dbfs
# ---------------------------------------------------------------------------

SUMMARY="${TMPDIR}/summary.tsv"
: > "$SUMMARY"

for i in "${!SOURCES[@]}"; do
    mic=$((i + 1))
    source="${SOURCES[$i]}"

    for freq in $(printf '%s\n' "${FREQUENCIES[@]}" | sort -n -u); do
        VALUES="${TMPDIR}/values-m${mic}-${freq}"

        awk \
            -F '\t' \
            -v m="$mic" \
            -v f="$freq" \
            '$1 == m && $3 == f { print $6 }' \
            "$RAW" |
            sort -n \
            > "$VALUES"

        count="$(wc -l < "$VALUES")"

        if (( count < 3 )); then
            echo "Not enough measurements for Mic $mic at ${freq} Hz" >&2
            exit 1
        fi

        lowest="$(head -n 1 "$VALUES")"
        highest="$(tail -n 1 "$VALUES")"

        trimmed_mean="$(
            sed '1d;$d' "$VALUES" |
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
            '%d\t%s\t%d\t%s\t%s\t%s\n' \
            "$mic" \
            "$source" \
            "$freq" \
            "$trimmed_mean" \
            "$lowest" \
            "$highest" \
            >> "$SUMMARY"
    done
done

# ---------------------------------------------------------------------------
# Normalize every microphone independently.
#
# This is crucial: microphones/interfaces can have different absolute gains.
#
# Columns:
#
# 1 mic_id
# 2 source_name
# 3 frequency
# 4 trimmed_mean_dbfs
# 5 normalized_db
# 6 lowest_dbfs
# 7 highest_dbfs
# 8 normalization_reference_dbfs
# ---------------------------------------------------------------------------

NORMALIZED="${TMPDIR}/normalized.tsv"
: > "$NORMALIZED"

declare -A NORM_REFERENCES

for i in "${!SOURCES[@]}"; do
    mic=$((i + 1))
    source="${SOURCES[$i]}"

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
            "No measured frequencies for Mic $mic fall inside normalization band ${NORM_MIN}-${NORM_MAX} Hz." \
            >&2
        exit 1
    }

    NORM_REFERENCES[$mic]="$ref"

    while IFS=$'\t' read -r row_mic row_source freq mean lowest highest; do
        [[ "$row_mic" == "$mic" ]] || continue

        normalized="$(
            awk \
                -v value="$mean" \
                -v reference="$ref" \
                'BEGIN { printf "%.3f", value - reference }'
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
done

# ---------------------------------------------------------------------------
# Average normalized response across microphones.
#
# Important: average the NORMALIZED responses, not raw dBFS.
#
# Columns:
#
# 1 frequency
# 2 average_normalized_db
# 3 between_microphone_sd_db
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

            variance = (sumsq / n) - (mean * mean)

            if (variance < 0)
                variance = 0

            sd = sqrt(variance)

            printf "%d\t%.3f\t%.3f\n", f, mean, sd
        }
    ' "$NORMALIZED" >> "$AVERAGE"

done

# ---------------------------------------------------------------------------
# Terminal summary
# ---------------------------------------------------------------------------

echo
echo "Normalization references:"

for i in "${!SOURCES[@]}"; do
    mic=$((i + 1))

    printf \
        "  Mic %-2d  %8.2f dBFS  (%d-%d Hz)\n" \
        "$mic" \
        "${NORM_REFERENCES[$mic]}" \
        "$NORM_MIN" \
        "$NORM_MAX"
done

echo
printf "%10s  %10s" "Frequency" "Average"

for i in "${!SOURCES[@]}"; do
    printf "  %10s" "Mic $((i + 1))"
done

printf "  %10s\n" "Mic SD"

for freq in $(printf '%s\n' "${FREQUENCIES[@]}" | sort -n -u); do

    avg="$(
        awk \
            -F '\t' \
            -v f="$freq" \
            '$1 == f { print $2 }' \
            "$AVERAGE"
    )"

    sd="$(
        awk \
            -F '\t' \
            -v f="$freq" \
            '$1 == f { print $3 }' \
            "$AVERAGE"
    )"

    printf "%8d Hz  %+8.2f" "$freq" "$avg"

    for i in "${!SOURCES[@]}"; do
        mic=$((i + 1))

        value="$(
            awk \
                -F '\t' \
                -v m="$mic" \
                -v f="$freq" \
                '$1 == m && $3 == f { print $5 }' \
                "$NORMALIZED"
        )"

        printf "  %+8.2f" "$value"
    done

    printf "  %8.2f\n" "$sd"
done

# ---------------------------------------------------------------------------
# CSV
#
# One row per actual microphone measurement.
#
# The derived per-frequency values are repeated for convenience.
# ---------------------------------------------------------------------------

{
    echo "name,sequence,frequency_hz,repetition,mic_id,source_name,rms_dbfs,trimmed_mean_dbfs,normalized_db,normalized_average_db,between_microphone_sd_db,lowest_dbfs,highest_dbfs,normalization_min_hz,normalization_max_hz,normalization_reference_dbfs"

    sort -t $'\t' -k4,4n -k1,1n "$RAW" |
    while IFS=$'\t' read -r mic source freq sequence repetition db; do

        normalized_row="$(
            awk \
                -F '\t' \
                -v m="$mic" \
                -v f="$freq" \
                '$1 == m && $3 == f {
                    print $4 "\t" $5 "\t" $6 "\t" $7 "\t" $8
                }' \
                "$NORMALIZED"
        )"

        mean="$(printf '%s\n' "$normalized_row" | cut -f1)"
        normalized="$(printf '%s\n' "$normalized_row" | cut -f2)"
        lowest="$(printf '%s\n' "$normalized_row" | cut -f3)"
        highest="$(printf '%s\n' "$normalized_row" | cut -f4)"
        norm_ref="$(printf '%s\n' "$normalized_row" | cut -f5)"

        average_row="$(
            awk \
                -F '\t' \
                -v f="$freq" \
                '$1 == f { print $2 "\t" $3 }' \
                "$AVERAGE"
        )"

        average="$(printf '%s\n' "$average_row" | cut -f1)"
        mic_sd="$(printf '%s\n' "$average_row" | cut -f2)"

        escaped_name="${NAME//\"/\"\"}"
        escaped_source="${source//\"/\"\"}"

        printf \
            '"%s",%d,%d,%d,%d,"%s",%s,%s,%s,%s,%s,%s,%s,%d,%d,%s\n' \
            "$escaped_name" \
            "$sequence" \
            "$freq" \
            "$repetition" \
            "$mic" \
            "$escaped_source" \
            "$db" \
            "$mean" \
            "$normalized" \
            "$average" \
            "$mic_sd" \
            "$lowest" \
            "$highest" \
            "$NORM_MIN" \
            "$NORM_MAX" \
            "$norm_ref"

    done
} > "$CSV"

# ---------------------------------------------------------------------------
# Determine graph ranges
# ---------------------------------------------------------------------------

MIN_FREQ="$(
    awk -F '\t' '
        NR == 1 || $3 < min { min = $3 }
        END { print min }
    ' "$NORMALIZED"
)"

MAX_FREQ="$(
    awk -F '\t' '
        NR == 1 || $3 > max { max = $3 }
        END { print max }
    ' "$NORMALIZED"
)"

MIN_DB="$(
    awk -F '\t' '
        NR == 1 || $5 < min { min = $5 }
        END { print min }
    ' "$NORMALIZED"
)"

MAX_DB="$(
    awk -F '\t' '
        NR == 1 || $5 > max { max = $5 }
        END { print max }
    ' "$NORMALIZED"
)"

AVG_MIN="$(
    awk -F '\t' '
        NR == 1 || $2 < min { min = $2 }
        END { print min }
    ' "$AVERAGE"
)"

AVG_MAX="$(
    awk -F '\t' '
        NR == 1 || $2 > max { max = $2 }
        END { print max }
    ' "$AVERAGE"
)"

MIN_DB="$(
    awk \
        -v a="$MIN_DB" \
        -v b="$AVG_MIN" \
        'BEGIN { print (a < b ? a : b) }'
)"

MAX_DB="$(
    awk \
        -v a="$MAX_DB" \
        -v b="$AVG_MAX" \
        'BEGIN { print (a > b ? a : b) }'
)"

Y_MIN="$(
    awk -v x="$MIN_DB" '
        BEGIN {
            print int((x - 5) / 5) * 5
        }
    '
)"

Y_MAX="$(
    awk -v x="$MAX_DB" '
        BEGIN {
            print int((x + 10) / 5) * 5
        }
    '
)"

if (( Y_MAX <= Y_MIN )); then
    Y_MAX=$((Y_MIN + 10))
fi

# ---------------------------------------------------------------------------
# Graph helpers
# ---------------------------------------------------------------------------

graph_x() {
    local freq="$1"

    awk \
        -v f="$freq" \
        -v min="$MIN_FREQ" \
        -v max="$MAX_FREQ" \
        -v left="$X_LEFT" \
        -v right="$X_RIGHT" '
        BEGIN {
            if (max == min)
                printf "%.2f", left
            else
                printf "%.2f", left + (right - left) * ((log(f) - log(min)) / (log(max) - log(min)))
        }
    '
}

graph_y() {
    local db="$1"

    awk \
        -v db="$db" \
        -v min="$Y_MIN" \
        -v max="$Y_MAX" \
        -v bottom="$Y_BOTTOM" \
        -v top="$Y_TOP" '
        BEGIN {
            printf "%.2f", bottom + (top - bottom) * ((db - min) / (max - min))
        }
    '
}

# Graphviz colors used cyclically if there are many sources.
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
# Build Graphviz plot
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
        label="",
        fontsize=9
    ];

    edge [
        penwidth=1.5
    ];

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

    # ---------------------------------------------------------------
    # Horizontal dB grid
    # ---------------------------------------------------------------

    tick="$Y_MIN"

    while (( tick <= Y_MAX )); do
        y="$(graph_y "$tick")"
        id=$((tick - Y_MIN))

        printf \
            '    ylabel_%d [shape=plaintext, pos="%d,%s!", label="%+d dB", fontsize=10];\n' \
            "$id" \
            "$((X_LEFT - 60))" \
            "$y" \
            "$tick"

        printf \
            '    gy_l_%d [shape=point, width=0, pos="%d,%s!"];\n' \
            "$id" \
            "$X_LEFT" \
            "$y"

        printf \
            '    gy_r_%d [shape=point, width=0, pos="%d,%s!"];\n' \
            "$id" \
            "$X_RIGHT" \
            "$y"

        if (( tick == 0 )); then
            printf \
                '    gy_l_%d -- gy_r_%d [color="#777777", style=dashed, penwidth=1.5];\n' \
                "$id" \
                "$id"
        else
            printf \
                '    gy_l_%d -- gy_r_%d [color="#dddddd", style=dotted, penwidth=0.6];\n' \
                "$id" \
                "$id"
        fi

        tick=$((tick + 5))
    done

    # ---------------------------------------------------------------
    # Frequency ticks / vertical grid
    # ---------------------------------------------------------------

    X_TICKS=(30 40 50 60 80 100 120 150 200 300 440 600 800)

    for freq in "${X_TICKS[@]}"; do
        if (( freq < MIN_FREQ || freq > MAX_FREQ )); then
            continue
        fi

        x="$(graph_x "$freq")"

        printf \
            '    xlabel_%d [shape=plaintext, pos="%s,%d!", label="%d", fontsize=9];\n' \
            "$freq" \
            "$x" \
            "$((Y_BOTTOM - 35))" \
            "$freq"

        printf \
            '    gx_b_%d [shape=point, width=0, pos="%s,%d!"];\n' \
            "$freq" \
            "$x" \
            "$Y_BOTTOM"

        printf \
            '    gx_t_%d [shape=point, width=0, pos="%s,%d!"];\n' \
            "$freq" \
            "$x" \
            "$Y_TOP"

        printf \
            '    gx_b_%d -- gx_t_%d [color="#eeeeee", style=dotted, penwidth=0.5];\n' \
            "$freq" \
            "$freq"
    done

    printf \
        '    frequency_label [shape=plaintext, pos="%d,%d!", label="Frequency [Hz]", fontsize=11];\n' \
        "$(((X_LEFT + X_RIGHT) / 2))" \
        "$((Y_BOTTOM - 70))"

    # ---------------------------------------------------------------
    # Individual microphone curves
    # ---------------------------------------------------------------

    for i in "${!SOURCES[@]}"; do
        mic=$((i + 1))
        color="${COLORS[$((i % ${#COLORS[@]}))]}"

        previous=""

        while IFS=$'\t' read -r row_mic source freq mean normalized lowest highest ref; do
            [[ "$row_mic" == "$mic" ]] || continue

            x="$(graph_x "$freq")"
            y="$(graph_y "$normalized")"

            node="m${mic}_f${freq}"

            printf \
                '    %s [pos="%s,%s!", color="%s", fillcolor="%s", style=filled];\n' \
                "$node" \
                "$x" \
                "$y" \
                "$color" \
                "$color"

            if [[ -n "$previous" ]]; then
                printf \
                    '    %s -- %s [color="%s", penwidth=1.6];\n' \
                    "$previous" \
                    "$node" \
                    "$color"
            fi

            previous="$node"

        done < "$NORMALIZED"
    done

    # ---------------------------------------------------------------
    # Average curve
    # ---------------------------------------------------------------

    previous=""

    while IFS=$'\t' read -r freq average sd; do
        x="$(graph_x "$freq")"
        y="$(graph_y "$average")"

        node="avg_f${freq}"

        printf \
            '    %s [pos="%s,%s!", color="black", fillcolor="black", style=filled, width=0.11, height=0.11];\n' \
            "$node" \
            "$x" \
            "$y"

        if [[ -n "$previous" ]]; then
            printf \
                '    %s -- %s [color="black", penwidth=3.0];\n' \
                "$previous" \
                "$node"
        fi

        previous="$node"

    done < "$AVERAGE"

    # ---------------------------------------------------------------
    # Legend
    # ---------------------------------------------------------------

    legend_x=$((X_RIGHT + 125))
    legend_y=$((Y_TOP - 20))

    printf \
        '    legend_title [shape=plaintext, pos="%d,%d!", label="Curves", fontsize=11];\n' \
        "$legend_x" \
        "$legend_y"

    legend_y=$((legend_y - 35))

    for i in "${!SOURCES[@]}"; do
        mic=$((i + 1))
        color="${COLORS[$((i % ${#COLORS[@]}))]}"

        printf \
            '    legend_m%d [shape=plaintext, pos="%d,%d!", label="Mic %d", fontcolor="%s", fontsize=10];\n' \
            "$mic" \
            "$legend_x" \
            "$legend_y" \
            "$mic" \
            "$color"

        legend_y=$((legend_y - 28))
    done

    printf \
        '    legend_average [shape=plaintext, pos="%d,%d!", label="Normalized average", fontcolor="black", fontsize=10];\n' \
        "$legend_x" \
        "$legend_y"

    echo "}"

} > "$DOT"

neato \
    -n2 \
    -Tpng \
    -Gdpi=150 \
    "$DOT" \
    -o "$PNG"

# ---------------------------------------------------------------------------
# Finished
# ---------------------------------------------------------------------------

echo
echo "Finished."
echo
echo "CSV: $CSV"
echo "PNG: $PNG"
echo "DOT: $DOT"
