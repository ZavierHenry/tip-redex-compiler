#!/usr/bin/env bash


case "$1" in
    build)
        case "$2" in
        *.smt2)
            tfile=$(mktemp "XXXXXXXXXXX.rkt")
            sed '1s|^|#lang s-exp "tip-syntax.rkt"\n|' "$2" > "$tfile"
            case "$3" in
            *.exe|*.app)
                raco exe -o "$3" "$tfile"
                ;;
            *.*)
                printf "\nUnsupported file: $3\n"
                printf "See '$0 build --help' for more help on build usage.\n"
                ;;
            *)
                raco exe -o "$3" "$tfile"
                ;;
            esac
            rm "$tfile"
            ;;
        --help)
            printf "\n\nUsage:\t$0 build [flags] [SOURCE] [DESTINATION]\n\n"
            printf "Builds the benchmark as a file type determinted by the destination filename format.\n\n"
            printf "Supported file types:\n\n"
            printf "\tNo file extension (Unix executable)\n"
            printf "\t.exe (Windows executable)\n"
            printf "\t.app (Apple application)\n\n"
            printf "List of flag options:\n\n"
            printf "\t--help:\tShows this help page\n\n"
            ;;
        *)
            printf "\nUnsupported source file.\n\nSee '$0 build --help' for more help on build usage.\n"
            ;;
        esac
    ;;
    run)
        output=""
        verbose=false
        timeset=false
        
        case "$2" in
        --help)
            printf "\n\nUsage:\t$0 run [flags and options] ...\n\n"
            printf "List of flags:\n\n"
            printf "\t-t [--time]:\t times the execution of the benchmark.\n"
            printf "\t-l [--log=FILEPATH]: Logs results to the given file.\n"
            printf "\t-v [--verbose]:\tPrints all results (only failed results are shown by default)\n"
            exit
            ;;
        esac
        
        while [[ $# -gt 2 ]]
        do
            case "$2" in
            -v|--verbose)
                verbose=true
                shift
                ;;
            -l)
                output="$3"
                shift 2
                ;;
            --log=*)
                output="${2##--log=}"
                shift
                ;;
            -t|--time)
                timeset=true
                shift
                ;;
            *)
                printf "\nUnknown run flag.\n\nSee '$0 run --help' for more help on run usage.\n"
                exit
                ;;
            esac
        done
        
        src=$(mktemp XXXXXXXXXXXX.rkt)
        sed '1s|^|#lang s-exp "tip-syntax.rkt"|' "$2" > "$src"
        
        if $timeset; then
            result=$( { time racket "$src" 2>&1 ; } 2>&1)
        else
            result=$(racket "$src" 2>&1)
        fi
        
        if $verbose && [[ -n "$output" ]]; then
            echo "$result" >> "$output"
        elif $verbose; then
            echo "$result"
        elif [[ ! "$result" =~ "no counterexamples" && output ]]; then
            echo "$result" >> "$output"
        elif [[ ! "$result" =~ "no counterexamples" ]]; then
            echo "$result"
        fi
        
        find . -type f -name "${src%%.rkt}\.*" -delete
        ;;
    help)
        printf "\n\nUsage:\t$0 command [arguments] FILEPATH\n\n"
        printf "List of commands:\n\n"
        printf "\tbuild\tBuilds benchmark based on the extension of the target file\n"
        printf "\trun\tBuilds and runs benchmark\n\n"
        printf "\thelp\tLoads the current help page\n\n"
        printf "For more help on a specific command, run \"$0 [command] --help\"\n"
        ;;
        
    *)
        echo "Unknown command:" "\"$1\""
        echo "Run '$0 help' for usage"
    
    ;;
esac
