#!/usr/bin/env bash


case "$1" in
    build)
        case "$2" in
        *.smt2)
            case "$3" in
            *.rkt)
            cp "$2" "$3"
            sed -i '1s/^/#lang s-exp "tip-syntax.rkt"\n/' "$3"
            raco expand "$3" > "$3"
            ;;
            *)
            raco exe -o "$3" "$2"
            raco distribute "src" "$2"
            ;;
            esac
            ;;
        --help)
            printf "\n\nUsage:\t$0 build [--help] [SOURCE FILE] [DESTINATION FILE]\n\n"
            printf "Builds the benchmark as a file type determinted by the destination filename format.\n\n"
            printf "Supported file types:\n\n"
            printf "\tNo file extension (Unix executable)\n"
            printf "\t.rkt (Racket file)\n"
            printf "\t.exe (Windows executable)\n"
            printf "\t.app (Apple application)\n"
            ;;
        *)
            printf "\nUnsupported source file.\n\nSee '$0 build --help' for more help on build usage.\n"
            ;;
        esac
    ;;
    run)
        output=&1
        verbose=false
        timeset=false
        
        while [[ $# -gt 2 ]]
        do
            case "$2" in
            -v|--verbose)
                verbose=true
                shift
                ;;
            -l)
                shift 2
                ;;
            --log=*)
                shift
                ;;
            -t|--time)
                timeset=true
                shift
            ;;
            --help)
                printf "\n\nUsage:\t$0 run [flags and options] ...\n\n"
                printf "List of flags:\n\n"
                printf "\t-t [--time]:\t times the execution of the benchmark.\n"
                printf "\t-l [--log=FILEPATH]: Logs results to the given file.\n"
                exit
                ;;
            *)
                printf "\nUnknown run flag.\n\nSee '$0 run --help' for more help on run usage.\n"
                exit
                ;;
            esac
        done
        
        if $timeset; then
            time racket "$2" &1>"$output" &2>1
        else
            racket "$2" 1>"$output" 2>&1
        fi
        
        # remove .dep, .bak, and .zo files
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
