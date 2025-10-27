;; Gaming-focused channel configuration
;; Includes nonguix for gaming software and proprietary drivers
;;
;; Useful aliases (add to your shell profile):
;; alias g='guix'
;; alias gs='guix shell'
;; alias gp='guix pull'
;; alias gi='guix install'
;; alias gu='guix upgrade'
;; alias gr='guix remove'
;; alias gd='guix describe'
;; alias gt='guix time-machine'

(cons* (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        (branch "master")
        (introduction
         (make-channel-introduction
          "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
          (openpgp-fingerprint
           "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
       %default-channels)
