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
