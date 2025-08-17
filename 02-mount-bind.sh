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
