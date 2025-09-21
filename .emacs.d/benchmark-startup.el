;;; benchmark-startup.el --- Benchmark Emacs startup time

(defun benchmark-emacs-startup ()
  "Benchmark Emacs startup time and report results."
  (interactive)
  (let ((start-time (current-time))
        (gc-count-start gcs-done))

    ;; Simulate a restart by measuring time to load configuration
    (message "Benchmarking Emacs startup performance...")

    ;; Display current startup metrics
    (with-output-to-temp-buffer "*Startup Benchmark*"
      (princ "Emacs Startup Performance Report\n")
      (princ "================================\n\n")

      (princ (format "Emacs Version: %s\n" emacs-version))
      (princ (format "System: %s\n" system-type))
      (princ (format "Configuration: %s\n\n" user-init-file))

      (if (boundp 'startup-time-start)
          (let ((startup-time (float-time (time-subtract after-init-time startup-time-start))))
            (princ (format "Last Startup Time: %.2f seconds\n" startup-time))
            (princ (format "Garbage Collections: %d\n\n" gcs-done)))
        (princ "Startup time measurement not available.\n\n"))

      ;; Package loading information
      (when (featurep 'use-package)
        (princ "Use-package Statistics:\n")
        (if (and (boundp 'use-package-statistics)
                 use-package-compute-statistics)
            (progn
              (princ (format "Packages configured: %d\n"
                           (length use-package-statistics)))
              (let ((total-time 0))
                (dolist (pkg use-package-statistics)
                  (setq total-time (+ total-time (or (cdr pkg) 0))))
                (princ (format "Total package load time: %.3f seconds\n" total-time))))
          (princ "Use-package statistics not enabled.\n"))
        (princ "\n"))

      ;; Memory usage
      (princ "Memory Usage:\n")
      (princ (format "GC threshold: %d bytes (%.1f MB)\n"
                   gc-cons-threshold
                   (/ gc-cons-threshold 1024.0 1024.0)))
      (princ (format "GC percentage: %.1f%%\n" (* gc-cons-percentage 100)))
      (princ "\n")

      ;; Recommendations
      (princ "Performance Tips:\n")
      (princ "- Run 'use-package-report' to see detailed package loading times\n")
      (princ "- Use 'profiler-start' and 'profiler-report' for detailed profiling\n")
      (princ "- Consider adding :defer t to slow-loading packages\n")
      (princ "- Check 'garbage-collect' stats after heavy operations\n"))

    (message "Benchmark complete. See *Startup Benchmark* buffer for details.")))

;; Run automatically if loaded interactively
(when (called-interactively-p 'any)
  (benchmark-emacs-startup))