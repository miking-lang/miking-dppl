# To run the experiments, we call a Python script, for details see scripts/run-plot-transform-results-cppl.py
# The experiment outputs a data file under "data_results/" that gives the normalizing constants for each particle number for <numruns> runs
usage() {
    echo "Usage: bash run-experiments.sh <model> <dataset> <numruns>"
    echo "Models:"
    echo "lda"
    echo "blr"
    echo "vbd"
    echo "crbd"
    echo "Datasets:"
    echo "lw"
    echo "c3"
    echo "nips40"
    echo "housing"
    echo "vbd-data"
    echo "crbd-data"
    echo "help"
    echo "e.g.,bash run-experiments.sh lda lw 30"
}
model="$1"
dataset="$2"
nrun="$3"
case "$dataset" in
    "lw")
        echo "Running lw data"
        case "$model" in
            "lda")
                echo "Model: lda"
                python3.12 scripts/run-plot-transform-results-cppl.py coreppl/models/experiments/lda/ --modelN lda --datasetName lw --filename lda-lw --lstPT 300 3000 30000 --lstP 300 3000 30000 --numruns $nrun
                ;;
            *)
                echo "Invalid model-data combination specified"
                usage
                exit 1
                ;;
        esac
        ;;
    "c3")
        echo "Running c3 data"
        case "$model" in
            "lda")
                echo "Model: lda"
                python scripts/run-plot-transform-results-cppl.py coreppl/models/experiments/lda/ --modelN lda --datasetName c3 --filename lda-c3 --lstPT 300 3000 30000 --lstP 300 3000 30000 --numruns $nrun
                ;;
            *)
                echo "Invalid model-data combination specified"
                usage
                exit 1
                ;;
        esac
        ;;
    "nips40")
        echo "Running nips40 data"
        case "$model" in
            "lda")
                echo "Model: lda"
                python scripts/run-plot-transform-results-cppl.py coreppl/models/experiments/lda/ --modelN lda --datasetName nips40 --filename lda-nips40 --lstPT 300 3000 30000 --lstP 300 3000 30000 --numruns $nrun
                ;;
            *)
                echo "Invalid model specified"
                usage
                exit 1
                ;;
        esac
        ;;
    "housing")
        echo "Running California housing data"
        case "$model" in
            "blr")
                echo "Model: blr"
                python scripts/run-plot-transform-results-cppl.py coreppl/models/experiments/blr/ --modelN blr --datasetName housing --filename blr --lstPT 300 3000 30000 --lstP 300 3000 30000 --numruns $nrun
                ;;
            *)
                echo "Invalid model specified"
                usage
                exit 1
                ;;
        esac
        ;;
    "vbd-data")
        echo "Running vbd data"
        case "$model" in
            "vbd")
                echo "Model: vbd"
                python scripts/run-plot-transform-results-cppl-no-static.py coreppl/models/experiments/vbd/ --modelN vbd --datasetName vbd --filename vbd --lstPT 3000 30000 300000 --lstP 3000 30000 300000 --numruns $nrun
                ;;
            *)
                echo "Invalid model specified"
                usage
                exit 1
                ;;
        esac
        ;;
        "crbd-data")
        echo "Running crbd data"
        case "$model" in
            "crbd")
                echo "Model: crbd"
                python scripts/run-plot-transform-results-cppl-no-static.py coreppl/models/experiments/crbd/ --modelN crbd --datasetName alcedinidae --filename crbd --lstPT 300 3000 30000 --lstP 300 3000 30000 --numruns $nrun
                ;;
            *)
                echo "Invalid model specified"
                usage
                exit 1
                ;;
        esac
        ;;
    *)
        echo "Invalid dataset specified"
        usage
        exit 1
        ;;
esac


