package main

import (
	"encoding/binary"
	"fmt"
	"net"
	"time"
)

func ipToInt32(ip string) (uint32, error) {
	parsedIP := net.ParseIP(ip)
	if parsedIP == nil {
		return 0, fmt.Errorf("invalid IP address: %s", ip)
	}
	ipv4 := parsedIP.To4()
	if ipv4 == nil {
		return 0, fmt.Errorf("not an IPv4 address: %s", ip)
	}
	return binary.BigEndian.Uint32(ipv4), nil
}

func main() {
	start := time.Now()

	ip := "128.32.10.1"
	ipInt32, err := ipToInt32(ip)
	if err != nil {
		fmt.Println(err)
		return
	}
	fmt.Printf("%s => %d\n", ip, ipInt32)

	elapsed := time.Since(start)
	fmt.Printf("Time elapsed: %s\n", elapsed)
}