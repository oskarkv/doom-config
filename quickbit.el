;;; quickbit.el -*- lexical-binding: t; -*-
(require 'utils)

(defmacro qb--shell-command-in-root (pre-command-string dir)
  `(ok-projectile-run-in-root
    (shell-command
     (printit (str ,pre-command-string
          ,'command " "
          (or ,'path ,dir))))))

(defmacro qb--if-in-path-do-command (path dir)
  `(if (ok-project-path-contains? ,path)
       (qb--shell-command-in-root
        (str "cd /home/oskar" ,path " && source venv/bin/activate && ")
        ,dir)))

(defun ok-quickbit-venv-command (command &optional path)
  (if (ok-project-path-contains? "/quickbit/kyc/")
      (ok-projectile-run-in-root
       (shell-command
        (str command " " (or path "kyc")))))
  (qb--if-in-path-do-command "/quickbit/merchant-service/" "merchant")
  (qb--if-in-path-do-command "/quickbit/monorepo/app-backend/" "quickbit")
  (qb--if-in-path-do-command "/quickbit/qb-python-utils/" "src tests")
  (qb--if-in-path-do-command "/quickbit/qb-backoffice-backend/" "backoffice")
  (qb--if-in-path-do-command "/quickbit/merchant-backoffice-backend/" "backoffice")
  (qb--if-in-path-do-command "/quickbit/qb-pay-backend/" "qb_pay")
  (qb--if-in-path-do-command "/quickbit/backoffice-core-backend/" "backoffice"))

(defun ok-python-black (&optional path)
  (println "path is" path)
  (interactive)
  (cond
   ((ok-project-path-contains? "/quickbit/kyc/")
    (ok-quickbit-venv-command "poetry run black kyc tests"))
   (t
    (ok-quickbit-venv-command "python3 -m black --exclude migrations" path))))

(defun ok-python-isort (path)
  (ok-quickbit-venv-command "isort" path))

(defun ok-python-import-pprint ()
  (interactive)
  (evil-with-single-undo
    (save-excursion
      (evil-goto-first-line)
      (while (evil-in-comment-p)
        (evil-next-line))
      (evil-open-above 1)
      (evil-normal-state)
      (insert "from quickbit_utils.misc import pprint"))))

(defun ok-test-function (command)
  (shell-command (str "jobbsetup " (projectile-project-root) " pytest " command)))

(defun ok-set-jedi-extra-paths ()
  (let ((project-root (projectile-project-root)))
    (when project-root
      (cond ((s-contains? "merchant-backend" project-root)
             (setq lsp-jedi-workspace-extra-paths
                   (vector (concat (projectile-project-root)
                                   "merchant/core")
                           (concat (projectile-project-root)
                                   "merchant/api_gateway"))))))))

(provide 'quickbit)
