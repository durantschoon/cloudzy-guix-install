#!/bin/bash
# Script to fix network configuration on fresh Guix install
# Run this on the installed Guix system

echo "=== Network Configuration Fix ==="
echo ""

echo "1. Checking network interfaces:"
ip addr show 2>/dev/null || ifconfig 2>/dev/null || echo "  Network tools not available"
echo ""

echo "2. Checking network services:"
herd status 2>/dev/null | grep -E "(network|dhcp|NetworkManager)" || echo "  Checking systemd services..."
systemctl list-units --type=service 2>/dev/null | grep -E "(network|dhcp|NetworkManager)" || echo "  No network services found"
echo ""

echo "3. Checking if we can start NetworkManager:"
if herd status network-manager 2>/dev/null; then
    echo "  NetworkManager service exists"
    echo "  Attempting to start..."
    herd start network-manager
    sleep 3
    herd status network-manager
elif systemctl list-unit-files 2>/dev/null | grep -q network-manager; then
    echo "  NetworkManager unit file exists"
    systemctl start NetworkManager
    sleep 3
    systemctl status NetworkManager
else
    echo "  NetworkManager not found"
fi
echo ""

echo "4. Checking if we can use dhcpcd:"
if command -v dhcpcd 2>/dev/null; then
    echo "  dhcpcd is available"
    echo "  Attempting to start DHCP on all interfaces..."
    dhcpcd -n 2>&1 | head -10
elif command -v dhclient 2>/dev/null; then
    echo "  dhclient is available"
    echo "  Finding network interfaces..."
    ip link show | grep -E "^[0-9]+:" | awk '{print $2}' | sed 's/:$//' | head -3
else
    echo "  No DHCP client found"
fi
echo ""

echo "5. Manual network configuration (if needed):"
echo "  If automatic configuration doesn't work, try:"
echo "  - For VPS: Usually eth0 or ens3 or enp0s3"
echo "  - Start DHCP manually: dhcpcd eth0"
echo "  - Or configure static IP: ip addr add IP_ADDRESS dev eth0"
echo ""

echo "6. After network is up, test connectivity:"
echo "  ping -c 2 1.1.1.1"
echo "  ping -c 2 8.8.8.8"
echo ""

echo "=== Network Fix Complete ==="

