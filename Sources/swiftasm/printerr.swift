import Darwin 

// Use this if you want to see output in `swift test`
func printerr(_ message: String) {
    fputs("\(message)\n", stderr)
}