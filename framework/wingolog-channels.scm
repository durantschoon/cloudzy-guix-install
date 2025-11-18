;; wingolog-channels.scm
;; Channels pinned to the same era as Wingo's Framework 13 AMD writeup (2024-02-16).
;;
;; This provides a known-good combination of kernel, firmware, and AMD GPU support
;; for Framework 13 AMD laptops, avoiding the firmware loading failures seen with
;; newer guix/nonguix commits.
;;
;; Usage:
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
