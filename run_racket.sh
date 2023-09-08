filename=$1

# Run the racket file
racket $filename &&
    echo "\n" &&
    racket -i -e "(enter! \"$filename\")"
