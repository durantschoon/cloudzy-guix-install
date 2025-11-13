#!/usr/bin/env bash
set -euo pipefail

# Pre-build Guix kernel using Docker on Mac
# Avoids cow-store issues and substitute server unreliability during Framework 13 installation

echo "=== Pre-building Guix Kernel in Docker ==="
echo

# Check if running on macOS
if [[ "$(uname)" != "Darwin" ]]; then
    echo "[ERROR] This script is designed to run on macOS with Docker"
    exit 1
fi

# Create channels.scm for nonguix
CHANNELS_FILE="./channels-for-prebuild.scm"
cat > "$CHANNELS_FILE" <<'EOF'
(list (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        (introduction
          (make-channel-introduction
            "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
            (openpgp-fingerprint
              "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
      (channel
        (name 'guix)
        (url "https://git.savannah.gnu.org/git/guix.git")
        (introduction
          (make-channel-introduction
            "9edb3f66fd807b096b48283debdcddccfea34bad"
            (openpgp-fingerprint
              "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))
EOF

echo "[INFO] Created $CHANNELS_FILE for nonguix channel"
echo

# Start Guix container and build kernel
echo "[INFO] Starting Docker container with Guix..."
echo "[INFO] This will download/build the Linux kernel (may take 30-60 minutes)"
echo

# Run guix in Docker, mounting current directory
docker run -it --rm \
    -v "$(pwd):/mnt/work" \
    -w /mnt/work \
    guix/guix:latest \
    bash -c "
        set -euo pipefail

        echo '[INFO] Authorizing nonguix substitute server...'
        guix archive --authorize < /mnt/work/nonguix-signing-key.pub || true

        echo '[INFO] Building kernel with time-machine...'
        guix time-machine -C /mnt/work/$CHANNELS_FILE -- build linux --no-grafts

        echo
        echo '[INFO] Finding kernel store path...'
        KERNEL_PATH=\$(guix time-machine -C /mnt/work/$CHANNELS_FILE -- build linux --no-grafts)

        echo '[INFO] Kernel built at:' \$KERNEL_PATH
        echo

        echo '[INFO] Exporting kernel to NAR archive...'
        guix archive --export \$KERNEL_PATH > /mnt/work/linux-kernel.nar

        echo
        echo '[OK] Kernel exported to linux-kernel.nar'
        ls -lh /mnt/work/linux-kernel.nar
    "

echo
echo "=== Pre-build Complete ==="
echo
echo "Next steps:"
echo "  1. Transfer linux-kernel.nar to Framework 13 via USB or network"
echo "  2. Boot Framework 13 from Guix ISO"
echo "  3. Import kernel: guix archive --import < linux-kernel.nar"
echo "  4. Run installer normally - it will use the pre-built kernel"
echo
echo "Transfer example (USB):"
echo "  cp linux-kernel.nar /Volumes/USB_DRIVE/"
echo
echo "Transfer example (network):"
echo "  # On Mac:"
echo "  python3 -m http.server 8000"
echo "  # On Framework 13 Guix ISO:"
echo "  wget http://YOUR_MAC_IP:8000/linux-kernel.nar"
echo "  guix archive --import < linux-kernel.nar"
