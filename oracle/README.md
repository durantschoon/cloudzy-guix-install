# Guix System on Oracle Cloud Infrastructure (Always Free)

⚠️ **Status: the image definition is written and validated, but nothing here has been booted yet.** Commands below marked *(untested)* are reasoned from the OCI docs, not from a successful run. Treat this as a working draft.

## How this platform differs from the others

Every other platform in this repo boots the Guix live ISO and runs `guix system init`. **OCI cannot boot an ISO** — it only accepts QCOW2/VMDK custom images uploaded to Object Storage.

So there is no bootstrap script, no numbered install steps, and no Go code here. The whole installer is one declarative file, `image/oracle-image.scm`, built locally and uploaded.

```
guix system image  ->  Object Storage  ->  custom image  ->  instance
   (your machine)         (upload)          (import)        (launch)
```

## Prerequisites

- Guix on x86_64 (verified with `17c2142`)
- `oci` CLI configured — `oci iam region-subscription list` should return your regions
- Your SSH **public** key at `oracle/image/authorized-key.pub`

**The SSH key is baked into the image.** Guix has no cloud-init, so there is no way to inject a key at launch. Get it wrong and the instance is unreachable except via the serial console — password auth is disabled by design.

```bash
cp ~/.ssh/id_ed25519.pub oracle/image/authorized-key.pub
```

## 1. Build the image

```bash
guix system image -t qcow2 --image-size=50G oracle/image/oracle-image.scm
```

Prints a store path ending in `image.qcow2`. `--image-size=50G` makes the root partition already span the OCI boot volume, which is why no first-boot partition-growth service is needed. The qcow2 is compressed and sparse, so the upload stays small despite the nominal size.

## 2. Smoke-test locally before uploading

Strongly recommended — an hour of upload plus import is a slow way to discover the image does not boot.

```bash
IMG=$(guix system image -t qcow2 --image-size=50G oracle/image/oracle-image.scm)
cp "$IMG" /tmp/guix-oracle.qcow2 && chmod +w /tmp/guix-oracle.qcow2   # store is read-only
qemu-system-x86_64 -m 2048 -drive file=/tmp/guix-oracle.qcow2,format=qcow2 -nographic
```

`-nographic` routes everything to the serial line, which is exactly what OCI's console does — so this also verifies the `console=ttyS0` configuration. You should see the GRUB menu, then a login prompt. Exit QEMU with `Ctrl-a x`.

## 3. Upload to Object Storage *(untested)*

```bash
COMPARTMENT=$(oci iam compartment list --query 'data[0]."compartment-id"' --raw-output)  # or your tenancy OCID
NAMESPACE=$(oci os ns get --query data --raw-output)

oci os bucket create --name guix-images --compartment-id "$COMPARTMENT"
oci os object put --bucket-name guix-images --name guix-oracle.qcow2 \
                  --file /tmp/guix-oracle.qcow2
```

## 4. Import as a custom image *(untested)*

```bash
oci compute image import from-object \
    --compartment-id "$COMPARTMENT" \
    --namespace "$NAMESPACE" \
    --bucket-name guix-images \
    --name guix-oracle.qcow2 \
    --display-name guix-oracle \
    --source-image-type QCOW2 \
    --launch-mode PARAVIRTUALIZED \
    --operating-system "Guix System" \
    --operating-system-version "rolling"
```

**`--launch-mode PARAVIRTUALIZED` is not optional.** It must match the BIOS/MBR layout the `qcow2` image type produces. `NATIVE` (UEFI) would require building `-t qcow2-gpt` with `grub-efi-bootloader` instead.

Import is asynchronous. Poll until `AVAILABLE`:

```bash
oci compute image list --compartment-id "$COMPARTMENT" \
    --display-name guix-oracle \
    --query 'data[0].{state:"lifecycle-state",id:id}' --output table
```

## 5. Launch *(untested)*

Needs a VCN with a public subnet. The console's **Create VCN with Internet Connectivity** wizard is the fast path; then:

```bash
oci compute instance launch \
    --compartment-id "$COMPARTMENT" \
    --availability-domain "$(oci iam availability-domain list --query 'data[0].name' --raw-output)" \
    --shape VM.Standard.E2.1.Micro \
    --image-id <image-ocid> \
    --subnet-id <subnet-ocid> \
    --assign-public-ip true \
    --display-name guix-oracle
```

Note there is **no `--metadata ssh_authorized_keys`** — that only works for images running cloud-init. The key is already in the image.

Open port 22 in the subnet's security list, then:

```bash
ssh guix@<public-ip>
```

## If it does not boot

Use the OCI serial console (Instance → Console connection). The image is configured to put GRUB and a login prompt on `ttyS0` precisely for this.

Most likely causes, in order:

1. **Launch mode mismatch** — `PARAVIRTUALIZED` vs. the image's BIOS/MBR layout
2. **Wrong SSH key baked in** — boots fine, refuses your login
3. **Boot volume is `/dev/sda`, not `/dev/vda`** — boots fine, but the *first* `guix system reconfigure` fails installing GRUB. Check `lsblk` and update `(targets ...)` in `image/oracle-image.scm`.

## Design notes

`image/oracle-image_purpose.txt` explains every setting and, more usefully, several things deliberately left out — why `initrd-modules` is absent, why the root label must stay `Guix_image`, why swap is a shepherd service rather than `swap-devices`, and why `%wheel ALL=NOPASSWD:ALL` is a consequence of key-only login rather than laziness.
