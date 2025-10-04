
## Partition Layout for Dual Boot

| Partition                      | Size (suggested)                                | Filesystem                     | Purpose                                                                                  |
| ------------------------------ | ----------------------------------------------- | ------------------------------ | ---------------------------------------------------------------------------------------- |
| **EFI System Partition (ESP)** | 512 MB–1 GB                                     | FAT32                          | Shared UEFI bootloader storage (Pop!_OS will create it; Guix will add its entries here). |
| **Pop!_OS Root (`/`)**         | 60–100 GB (or more if you want most space here) | ext4 (default)                 | Main OS partition for Pop!_OS system files.                                              |
| **Guix Root (`/`)**            | 40–60 GB (or more if you plan heavy use)        | ext4, btrfs, or Guix-preferred | Main OS partition for Guix.                                                              |
| **Swap**                       | 8–32 GB (depends on RAM and hibernate plans)    | swap                           | For suspend-to-disk/extra memory. Can be shared between distros.                         |
| **Home / Data**                | Remainder                                       | ext4 or btrfs                  | Shared storage for your personal files; both Pop!_OS and Guix can mount it.              |
| _(Optional)_ Boot (`/boot`)    | 1–2 GB                                          | ext4                           | Sometimes useful to separate kernel images if Guix gives you trouble with ESP.           |
