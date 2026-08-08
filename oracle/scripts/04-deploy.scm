#!/run/current-system/profile/bin/guile --no-auto-compile
!#
;;; 04-deploy.scm --- upload, import, network, launch on OCI.
;;;
;;; Encodes the exact sequence first run successfully on 2026-08-08:
;;;
;;;   Object Storage bucket -> multipart upload -> custom image import
;;;   (PARAVIRTUALIZED) -> poll AVAILABLE -> VCN + internet gateway +
;;;   default route + public subnet -> launch VM.Standard.E2.1.Micro
;;;   with a public IP -> wait for the SSH banner.
;;;
;;; Idempotent throughout: every resource is looked up by display-name
;;; first and only created if absent, so a rerun after any failure
;;; continues instead of duplicating.  All state lives in OCI itself;
;;; nothing is stored locally between runs.
;;;
;;; No JSON parser needed: every oci call uses --query/--raw-output.
;;;
;;; The final SSH login is left to the user on purpose: the instance
;;; only trusts the key baked into the image, which is normally the
;;; user's passphrase-protected personal key.  This script verifies the
;;; SSH BANNER (sshd up, port reachable) -- an unattended full login
;;; check would need the private key unencrypted, which we do not ask
;;; for.  See 03-smoke-test.scm for why "Permission denied (publickey)"
;;; from a BatchMode probe is SUCCESS here, not failure.

(load (string-append (dirname (car (command-line))) "/oci-common.scm"))

(define %bucket "guix-images")
(define %object-name "guix-oracle.qcow2")
(define %image-name "guix-oracle")
(define %instance-name "guix-oracle")
(define %vcn-name "guix-vcn")
(define %shape "VM.Standard.E2.1.Micro")

(define (compartment)
  "The root compartment is the tenancy; free-tier resources live there."
  (or (oci-config-value "tenancy")
      (die "no tenancy in ~/.oci/config; run 01-setup-client.scm first")))

(define (ocid-or-false s)
  "Treat empty/None query output as #f."
  (and (not (string-null? s)) (not (string=? s "None")) s))

;;; ---------------------------------------------------------------------
;;; Storage + image

(define (ensure-bucket)
  (if (command-succeeds?
       (string-append %oci-cli " os bucket get --bucket-name " %bucket " >/dev/null"))
      (say "[OK] bucket " %bucket " exists")
      (begin
        (oci (string-append "os bucket create --name " %bucket
                            " --compartment-id " (compartment) " >/dev/null"))
        (say "[OK] bucket " %bucket " created"))))

(define (upload-image image-path)
  "Multipart-upload the image.  --force overwrites a previous object, so
re-deploying a rebuilt image needs no manual cleanup."
  (say "Uploading " image-path " (a few minutes)...")
  (call-with-values
      (lambda ()
        (oci/status (string-append
                     "os object put --bucket-name " %bucket
                     " --name " %object-name
                     " --file " (sh-quote image-path)
                     " --part-size 128 --parallel-upload-count 4 --force"
                     " >/dev/null")))
    (lambda (output status)
      (if (zero? status)
          (say "[OK] uploaded as " %object-name)
          (die "upload failed; rerun (multipart uploads resume poorly, "
               "but the bucket and everything before this step are kept)")))))

(define (existing-available-image)
  "OCID of an AVAILABLE custom image named guix-oracle, or #f."
  (ocid-or-false
   (oci (string-append
         "compute image list --compartment-id " (compartment)
         " --display-name " %image-name
         " --lifecycle-state AVAILABLE"
         " --query 'data[0].id' --raw-output 2>/dev/null"))))

(define (ensure-imported-image)
  "Import the uploaded object as a custom image and wait for AVAILABLE.
PARAVIRTUALIZED launch mode is NOT optional: it must match the BIOS/MBR
layout the qcow2 image type produces (NATIVE would need qcow2-gpt +
grub-efi-bootloader)."
  (or (existing-available-image)
      (let ((namespace (oci "os ns get --query data --raw-output")))
        (say "Importing as custom image (takes ~5-20 minutes)...")
        (oci (string-append
              "compute image import from-object"
              " --compartment-id " (compartment)
              " --namespace " namespace
              " --bucket-name " %bucket
              " --name " %object-name
              " --display-name " %image-name
              " --source-image-type QCOW2"
              " --launch-mode PARAVIRTUALIZED"
              " --operating-system \"Guix System\""
              " --operating-system-version rolling >/dev/null"))
        (or (poll-until "image import to reach AVAILABLE"
                        existing-available-image
                        60 3600)
            (die "image import did not reach AVAILABLE within an hour; "
                 "check Compute -> Custom Images in the console")))))

;;; ---------------------------------------------------------------------
;;; Network (the CLI equivalent of the console's "Create VCN with
;;; Internet Connectivity" wizard; the default security list already
;;; allows SSH ingress on 22)

(define (ensure-network)
  "Return the OCID of a public subnet inside a VCN with internet access."
  (let ((vcn (ocid-or-false
              (oci (string-append
                    "network vcn list --compartment-id " (compartment)
                    " --display-name " %vcn-name
                    " --query 'data[0].id' --raw-output 2>/dev/null")))))
    (if vcn
        (begin
          (say "[OK] VCN " %vcn-name " exists")
          (ocid-or-false
           (oci (string-append
                 "network subnet list --compartment-id " (compartment)
                 " --vcn-id " vcn
                 " --query 'data[0].id' --raw-output"))))
        (let* ((vcn (oci (string-append
                          "network vcn create --compartment-id " (compartment)
                          " --display-name " %vcn-name
                          " --cidr-blocks '[\"10.0.0.0/16\"]'"
                          " --query data.id --raw-output")))
               (igw (oci (string-append
                          "network internet-gateway create"
                          " --compartment-id " (compartment)
                          " --vcn-id " vcn
                          " --is-enabled true --display-name guix-igw"
                          " --query data.id --raw-output")))
               (route-table (oci (string-append
                                  "network vcn get --vcn-id " vcn
                                  " --query 'data.\"default-route-table-id\"'"
                                  " --raw-output"))))
          (oci (string-append
                "network route-table update --rt-id " route-table
                " --route-rules '[{\"destination\":\"0.0.0.0/0\","
                "\"destinationType\":\"CIDR_BLOCK\","
                "\"networkEntityId\":\"" igw "\"}]'"
                " --force >/dev/null"))
          (let ((subnet (oci (string-append
                              "network subnet create"
                              " --compartment-id " (compartment)
                              " --vcn-id " vcn
                              " --cidr-block 10.0.0.0/24"
                              " --display-name guix-public-subnet"
                              " --query data.id --raw-output"))))
            (say "[OK] created VCN, internet gateway, route, public subnet")
            subnet)))))

;;; ---------------------------------------------------------------------
;;; Instance

(define (existing-instance)
  "OCID of a RUNNING/PROVISIONING/STARTING instance named guix-oracle, or #f."
  (ocid-or-false
   (oci (string-append
         "compute instance list --compartment-id " (compartment)
         " --display-name " %instance-name
         " --query '(data[?\"lifecycle-state\"==`RUNNING`"
         " || \"lifecycle-state\"==`PROVISIONING`"
         " || \"lifecycle-state\"==`STARTING`])[0].id'"
         " --raw-output 2>/dev/null"))))

(define (ensure-instance image-ocid subnet-ocid)
  (or (existing-instance)
      (let ((availability-domain
             (oci "iam availability-domain list --query 'data[0].name' --raw-output")))
        (say "Launching " %shape " ...")
        ;; No --metadata ssh_authorized_keys: that needs cloud-init, which
        ;; Guix does not run.  The key is already baked into the image.
        (oci (string-append
              "compute instance launch"
              " --compartment-id " (compartment)
              " --availability-domain " (sh-quote availability-domain)
              " --shape " %shape
              " --image-id " image-ocid
              " --subnet-id " subnet-ocid
              " --assign-public-ip true"
              " --display-name " %instance-name
              " --query data.id --raw-output")))))

(define (wait-until-running instance-ocid)
  (or (poll-until "instance to reach RUNNING"
                  (lambda ()
                    (let ((state (oci (string-append
                                       "compute instance get --instance-id " instance-ocid
                                       " --query 'data.\"lifecycle-state\"' --raw-output"))))
                      (cond ((string=? state "RUNNING") #t)
                            ((member state '("TERMINATED" "TERMINATING"))
                             (die "instance entered " state))
                            (else #f))))
                  20 900)
      (die "instance not RUNNING after 15 minutes")))

(define (public-ip-of instance-ocid)
  (oci (string-append "compute instance list-vnics --instance-id " instance-ocid
                      " --query 'data[0].\"public-ip\"' --raw-output")))

(define (wait-for-ssh-banner ip)
  "sshd answering proves boot completed.  `Permission denied (publickey)'
is the SUCCESS signature here -- see the header comment."
  (or (poll-until (string-append "sshd on " ip ":22")
                  (lambda ()
                    (let ((probe (run-command
                                  (string-append
                                   "ssh -o BatchMode=yes -o StrictHostKeyChecking=no"
                                   " -o UserKnownHostsFile=/dev/null -o ConnectTimeout=5"
                                   " probe@" ip " true 2>&1"))))
                      (and (string-contains probe "denied") #t)))
                  20 600)
      (die "no SSH banner after 10 minutes; use the OCI serial console "
           "(Instance -> Console connection) -- the image mirrors GRUB "
           "and login onto ttyS0 for exactly this")))

;;; ---------------------------------------------------------------------

(define (main)
  (unless (oci-authenticated?)
    (die "oci CLI is not set up; run 01-setup-client.scm first"))
  (let ((image-path
         (if (> (length (command-line)) 1)
             (cadr (command-line))
             (die "usage: 04-deploy.scm /gnu/store/...-image.qcow2  "
                  "(the path printed by 02-build-image.scm; run "
                  "03-smoke-test.scm on it first)"))))
    (unless (file-exists? image-path)
      (die image-path " does not exist"))
    (ensure-bucket)
    (if (existing-available-image)
        (say "[OK] custom image already imported (delete it in the console "
             "to force a re-import of a rebuilt qcow2)")
        (upload-image image-path))
    (let* ((image-ocid (ensure-imported-image))
           (subnet-ocid (ensure-network))
           (instance-ocid (ensure-instance image-ocid subnet-ocid)))
      (wait-until-running instance-ocid)
      (let ((ip (public-ip-of instance-ocid)))
        (wait-for-ssh-banner ip)
        (say "")
        (say "[OK] Guix System is RUNNING on Oracle Cloud")
        (say "")
        (say "    ssh guix@" ip)
        (say "")
        (say "(uses the key baked at oracle/image/authorized-key.pub;")
        (say " you will be asked for that key's passphrase, if it has one)")
        (say "")
        (say "Before your first guix system reconfigure on the instance,")
        (say "confirm with lsblk that the boot volume is /dev/sda, matching")
        (say "(targets ...) in oracle/image/oracle-image.scm.  (Observed sda")
        (say "on VM.Standard.E2.1.Micro PARAVIRTUALIZED, 2026-08-08; only a")
        (say "different shape or launch mode should change it.)")))))

(main)
