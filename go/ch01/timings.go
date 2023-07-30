package main

import (
	"fmt"
	"math"
)

var millisecondsMultipliers = []int64{1e6, 60, 60, 24, 30, 12, 100}
var periods = []string{"second", "minute", "hour", "day", "month", "year", "century"}

type OrderFunc func(float64) float64

var orders = []string{"lg n", "√n", "n", "n lg n", "n²", "n³", "2^n", "n!"}

var orderFuncs = []OrderFunc{
	func(n float64) float64 { return math.Log10(n) },
	func(n float64) float64 { return math.Sqrt(n) },
	func(n float64) float64 { return n },
	func(n float64) float64 { return n * math.Log10(n) },
	func(n float64) float64 { return math.Pow(n, 2) },
	func(n float64) float64 { return math.Pow(n, 3) },
	func(n float64) float64 { return math.Pow(2.0, n) },
	func(n float64) float64 { return float64(factorial(int64(n))) },
}

func factorial(n int64) int64 {
	if n == 0 {
		return 1
	}
	return n * factorial(n-1)
}

func main() {
	timings := make(map[string]int64, len(millisecondsMultipliers))
	for i := 0; i < len(millisecondsMultipliers); i++ {
		var multiplier int64 = 1
		for j := 0; j <= i; j++ {
			multiplier *= millisecondsMultipliers[j]
		}
		timings[periods[i]] = multiplier
	}
	for o, order := range orders {
		for _, period := range periods {
			ms := timings[period]
			fn := orderFuncs[o]
			i, n := 0.0, 10.0
			for {
				if fn(n) < float64(ms) {
					i = n
					n *= 1.001
				} else {
					fmt.Printf("%s %s: n=%.3e\n", order, period, i)
					break
				}
			}
		}
	}
}
