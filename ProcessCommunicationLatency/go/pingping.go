package main

import (
	"flag"
	"fmt"
	"os"
	"runtime"
	"time"
)

var r = flag.Int("r", 1e5, "Number of repetitions")
var d = flag.Int("d", 1e5, "Data size in bytes")

var ch = make(chan byte)
var p1send = make(chan []byte, 100)
var p2send = make(chan []byte, 100)

func p(data []byte, r int, send chan<- []byte, rec <-chan []byte) {
	for i := 0; i < r; i++ {
		send <- data
		copy(data, <-rec)
	}
	ch <- 0
}

func main() {
	flag.Parse()
	if *r <= 0 {
		fmt.Fprintf(os.Stderr, "invalid number of repetitions")
		os.Exit(1)
	}

	if *d <= 0 {
		fmt.Fprintf(os.Stderr, "invalid size of data")
		os.Exit(1)
	}

	var data = make([]byte, *d)

	fmt.Printf("Size of data: %d\n", len(data))

	var m0 runtime.MemStats
	runtime.ReadMemStats(&m0)
	fmt.Printf("Used memory in beginning: %dMB\n", m0.Alloc/(1024*1024))

	t00 := time.Now().Unix()

	t0 := time.Now().UnixNano() / (int64(time.Millisecond) / int64(time.Nanosecond))

	go p(data, *r, p1send, p2send)
	go p(data, *r, p2send, p1send)

	// wait for the two processes to finish
	// alternatively use var wg sync.WaitGroup
	<-ch
	<-ch

	t11 := time.Now().Unix()

	t1 := time.Now().UnixNano() / (int64(time.Millisecond) / int64(time.Nanosecond))

	var m1 runtime.MemStats
	runtime.ReadMemStats(&m1)

	fmt.Printf("Time taken in seconds(no mlsecs): %d\n", t11-t00)
	fmt.Printf("Time taken in seconds: %f\n", float64(t1-t0)/1000)
	fmt.Printf("Used memory in end: %dMB\n", m1.Alloc/(1024*1024))
}
