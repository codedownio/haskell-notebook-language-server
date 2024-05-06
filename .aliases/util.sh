
confirm_execution() {
    PROMPT="$1"
    if [[ -z "$PROMPT" ]]; then
        PROMPT="confirm"
    fi

    while true; do
        read -p "Are you sure you want to proceed? Type '$PROMPT' to continue: " input
        if [[ $input == "$PROMPT" ]]; then
            echo "Proceeding..."
            break
        else
            echo "Invalid input. Please type '$PROMPT' to continue."
        fi
    done
}
