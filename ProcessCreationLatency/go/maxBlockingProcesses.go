package main

import (
    "flag"
    "fmt"
    "os"
    "runtime"
    "time"
)

var n = flag.Int("n", 1e5, "Number of goroutines to create")

var ch = make(chan byte)

func f() {
    <-ch // Block this goroutine
}

func main() {
    flag.Parse()
    if *n <= 0 {
            fmt.Fprintf(os.Stderr, "invalid number of goroutines")
            os.Exit(1)
    }

    var m0 runtime.MemStats

    runtime.ReadMemStats(&m0)
    fmt.Printf("Used memory in beginning: %dMB\n", m0.Alloc/(1024*1024))
    t0 := time.Now().UnixNano() / (int64(time.Millisecond)/int64(time.Nanosecond))
    for i := 0; i < *n; i++ {
            go f()
    }

    t1 := time.Now().UnixNano() / (int64(time.Millisecond)/int64(time.Nanosecond))

    var m1 runtime.MemStats
    runtime.ReadMemStats(&m1)

    fmt.Printf("Number of goroutines: %d\n", *n)
    fmt.Printf("Time taken in seconds: %f\n", float64(t1-t0)/1000)
    fmt.Printf("Used memory in end: %dMB\n", m1.Alloc/(1024*1024))
}