;; wingolog-channels.scm
;; Channels pinned to the same era as Wingo's Framework 13 AMD writeup (2024-02-16).
;;
;; !! DO NOT USE THIS ON A RYZEN AI 300 MACHINE !!
;;
;; Wingo's post is about the Framework 13 with a **Ryzen 7040** APU.  On that
;; machine this pin is a reasonable known-good snapshot.
;;
;; On a Framework 13 **Ryzen AI 300** (Strix Point, GPU 1002:1114) it is
;; actively harmful.  That silicon shipped in July 2024, ~5 months after these
;; commits, and its amdgpu firmware -- psp_14_0_4, gc_11_5_*, dcn_3_5_* -- does
;; not exist in a Feb 2024 linux-firmware.  Reconfiguring through this file
;; produces exactly the failure it is reputed to fix:
;;
;;   Direct firmware load for amdgpu/psp_14_0_4_toc.bin failed with error -2
;;   amdgpu: Fatal error during GPU init
;;
;; Check which machine you have before using this:
;;
;;   cat /sys/class/dmi/id/product_name
;;   lspci -nn | grep -iE 'vga|display'
;;
;; The framework-dual installer no longer uses this file.  It pins to a recent
;; commit pair instead -- see FrameworkDualGuixCommit in lib/common.go and
;; docs/CHANNEL_PINNING_POLICY.md.  This file is kept only for Ryzen 7040.
;;
;; Usage (Ryzen 7040 only):
;;   sudo guix time-machine -C wingolog-channels.scm -- \
;;     system reconfigure /path/to/your/config.scm
;;
;; See: https://wingolog.org/archives/2024/02/16/guix-on-the-framework-13-amd

(list
  (channel
    (name 'guix)
    (url "https://git.savannah.gnu.org/git/guix.git")
    (branch "master")
    ;; Commit from 2024-02-16 23:19:48 +0100
    (commit "91d80460296e2d5a01704d0f34fb966a45a165ae")
    (introduction
      (make-channel-introduction
       "9edb3f66fd807b096b48283debdcddccfea34bad"
       (openpgp-fingerprint
        "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
  (channel
    (name 'nonguix)
    (url "https://gitlab.com/nonguix/nonguix")
    (branch "master")
    ;; Commit from 2024-02-14 16:36:06 -0500
    ;; (closest commit before Wingo's post date)
    (commit "10318ef7dd53c946bae9ed63f7e0e8bb8941b6b1")
    ;; Enable signature verification (from nonguix docs)
    (introduction
      (make-channel-introduction
       "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
       (openpgp-fingerprint
        "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5")))))
