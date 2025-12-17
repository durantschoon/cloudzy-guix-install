#!/bin/bash
# Diagnostic script to investigate Guix build failures
# Run this on the installed Guix system

echo "=== Guix Build Failure Diagnosis ==="
echo ""

echo "1. Checking guix-daemon status:"
herd status guix-daemon 2>/dev/null || systemctl status guix-daemon 2>/dev/null || echo "  Could not check daemon status"
echo ""

echo "2. Checking disk space:"
df -h / /gnu/store /tmp 2>/dev/null | grep -E "(Filesystem|/dev)"
echo ""

echo "3. Checking network connectivity:"
ping -c 2 ci.guix.gnu.org 2>&1 | head -3
echo ""

echo "4. Checking substitute servers:"
guix weather curl 2>&1 | head -10 || echo "  guix weather failed"
echo ""

echo "5. Checking build log for curl failure:"
if [ -f /var/log/guix/drvs/39/0463zvbdvlzwr14v44jwr7y1243f55-curl-7.84.0.tar.xz.drv.gz ]; then
    echo "  Build log exists, showing last 20 lines:"
    zcat /var/log/guix/drvs/39/0463zvbdvlzwr14v44jwr7y1243f55-curl-7.84.0.tar.xz.drv.gz 2>/dev/null | tail -20 || \
    gunzip -c /var/log/guix/drvs/39/0463zvbdvlzwr14v44jwr7y1243f55-curl-7.84.0.tar.xz.drv.gz 2>/dev/null | tail -20
else
    echo "  Build log not found at expected location"
    echo "  Searching for curl build logs:"
    find /var/log/guix -name "*curl*" -type f 2>/dev/null | head -5
fi
echo ""

echo "6. Checking available disk space in /tmp:"
df -h /tmp | tail -1
echo ""

echo "7. Checking guix-daemon build users:"
if [ -f /etc/guix/build-users.conf ]; then
    echo "  Build users config exists"
    cat /etc/guix/build-users.conf
else
    echo "  Build users config not found"
fi
echo ""

echo "8. Trying to fetch substitutes with verbose output:"
guix build --dry-run curl 2>&1 | head -20
echo ""

echo "9. Checking if we can use fallback builds:"
echo "  Attempting: guix build --fallback --no-substitutes curl"
echo "  (This will take a while, but shows if fallback builds work)"
echo ""

echo "=== Diagnosis Complete ==="
echo ""
echo "Common fixes:"
echo "1. If substitutes aren't downloading: Check network/firewall"
echo "2. If builds are failing: Check disk space, build logs"
echo "3. If daemon issues: Restart guix-daemon: herd restart guix-daemon"
echo "4. Try with fallback: guix install --fallback curl"

