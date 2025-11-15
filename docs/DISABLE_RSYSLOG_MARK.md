# Disabling rsyslog MARK Messages

If you're seeing periodic "localhost -- MARK --" messages in your terminal every 20 minutes, these are keepalive messages from rsyslog.

## Quick Fix

Edit your `/etc/config.scm` and configure rsyslog to disable MARK messages:

```scheme
(use-modules (gnu services sysutils))

(services
 (modify-services %base-services
   (rsyslog-service-type config =>
     (rsyslog-configuration
      (inherit config)
      (config-file
       (rsyslog-configuration-file
        (config-version "3")
        (modules
         (list (rsyslog-module name "imuxsock")
               (rsyslog-module name "imklog")))
        (rules
         (list (rsyslog-rule
                (selector "*.info")
                (action (rsyslog-file-action "/var/log/messages")))))
        ;; Disable MARK messages (keepalive)
        (global-directives
         (list "$MarkMessagePeriod 0")))))))
```

## Simpler Alternative

If you don't need rsyslog at all, you can remove it entirely:

```scheme
(services
 (remove (lambda (service)
           (eq? (service-kind service) rsyslog-service-type))
         %base-services))
```

## Verify

After reconfiguring:

```bash
sudo guix system reconfigure /etc/config.scm
```

The MARK messages should stop appearing.
