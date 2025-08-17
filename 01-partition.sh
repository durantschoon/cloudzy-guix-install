export PATH=/run/current-system/profile/bin:/run/current-system/profile/sbin:/run/setuid-programs:$PATH

for d in /dev/sda /dev/vda /dev/nvme0n1; do [ -b "$d" ] && DEVICE=$d && break; done
echo "Using $DEVICE"

parted --script "$DEVICE" \
	mklabel gpt \
	mkpart ESP fat32 1MiB 513MiB \
	set 1 esp on \
	mkpart root ext4 513MiB 100%
	
EFI=${DEVICE}1
ROOT=${DEVICE}2

echo "EFI is $EFI and ROOT is $ROOT"

mkfs.vfat -F32 "$EFI"
mkfs.ext4 "$ROOT"

set -eu

mount | grep -q " on /mnt " || mount $ROOT /mnt

mkdir -p /mnt/gnu/store
mkdir -p /mnt/var/guix

if command -v herd >/dev/null 2>&1; then
  herd stop guix-daemon
else
  pkill -TERM -x guix-daemon
fi

start=$(date +%s)

if command -v rsync >/dev/null 2>&1; then
  rsync -aHAX --info=progress2 /gnu/store/. /mnt/gnu/store/.
else
  cp -a /gnu/store/. /mnt/gnu/store/
fi

end=$(date +%s)
echo "Time taken: $((end - start)) seconds"

mkdir -p /mnt/boot/efi
mount $EFI /mnt/boot/efi

mkdir -p /mnt/gnu /mnt/var/guix
herd stop guix-daemon

mount --bind /mnt/gnu /gnu
mount --bind /mnt/var/guix /var/guix

df -h /gnu /var/guix

herd start guix-daemon
