package main

import (
	"flag"
	"fmt"
	"os"
	"runtime"
	"time"
	"sync/atomic"
)

var n = flag.Int("n", 1e5, "Number of goroutines to create")

var ch = make(chan byte)
var starter = make(chan byte, 1000) 
var counter = 0
var aggregator = make(chan uint64, 100) 
func client(mailbox chan []byte) {
	message := make([]byte, 500)
	for {
		copy(message, <- mailbox)
		mailbox <- message
	}
}

func server(aggregator chan uint64, message []byte) {
	mailbox := make(chan []byte)
	go client(mailbox)
	<-starter

	var i uint64 = 0
	for {

		mailbox <- message
		copy(message[:], <-mailbox)
		
		i++
		if i == 10000 {
			aggregator <- i  //(max unread messages are bound to the buffer size of the channel)
			i = 0
		}
	}
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

	message := make([]byte, 500)
	for i, _ := range message {
    	message[i] = byte(i)
	}

	////////////////////////////// CREATING THE GOROUTINES ///////////////////////////////
	t0 := time.Now().UnixNano() / (int64(time.Millisecond) / int64(time.Nanosecond))
	for i := 0; i < *n; i++ {
		go server(aggregator, message)
	}

	t1 := time.Now().UnixNano() / (int64(time.Millisecond) / int64(time.Nanosecond))

	var m1 runtime.MemStats
	runtime.ReadMemStats(&m1)
	fmt.Printf("Creation of goroutines completed.\n")
	fmt.Printf("Number of goroutines: %d\n", *n)
	fmt.Printf("Time taken in seconds: %f\n", float64(t1-t0)/1000)
	fmt.Printf("Used memory in end: %dMB\n", m1.Alloc/(1024*1024))

	////////////////////////////// STARTING THE GOROUTINES ///////////////////////////////
	t00 := time.Now().UnixNano() / (int64(time.Millisecond) / int64(time.Nanosecond))

	for i := 0; i < *n; i++ {
		starter <- 1
	}

	var totalMessages uint64 = 0

	go func(){
		for{
			atomic.AddUint64(&totalMessages, <- aggregator)
		}
	}()

	time.Sleep(time.Second * 60)

	t11 := time.Now().UnixNano() / (int64(time.Millisecond) / int64(time.Nanosecond))
	fmt.Printf("Time taken in seconds: %f\n", float64(t11-t00)/1000)
    fmt.Printf("Total Messages: %d\n",atomic.LoadUint64(&totalMessages))

	os.Exit(0)
}
