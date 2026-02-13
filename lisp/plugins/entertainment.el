;; Entertainment-related configuration
(use-package emms
  :ensure t
  :config
  (require 'emms-setup)
  (emms-all)
  (emms-mode-line-disable)
  (emms-default-players)
  (setq emms-source-file-default-directory "~/Music/"))

;; Source: https://www.reddit.com/r/emacs/comments/qg2d0k/emms_modeline_shows_full_path_to_the_songs_i_only/
(defun track-title-from-file-name (file)
  "For using with EMMS description functions. Extracts the track
title from the file name FILE, which just means a) taking only
the file component at the end of the path, and b) removing any
file extension."
  (with-temp-buffer
    (save-excursion (insert (file-name-nondirectory (directory-file-name file))))
    (ignore-error 'search-failed
      (search-forward-regexp (rx "." (+ alnum) eol))
      (delete-region (match-beginning 0) (match-end 0)))
    (buffer-string)))

(defun my-emms-track-description (track)
  "Return a description of TRACK, for EMMS, but try to cut just
the track name from the file name, and just use the file name too
rather than the whole path."
  (let ((artist (emms-track-get track 'info-artist))
        (title (emms-track-get track 'info-title)))
    (cond ((and artist title)
           (concat (format "%s" artist) " - " (format "%s" title)))
          (title title)
          ((eq (emms-track-type track) 'file)
           (track-title-from-file-name (emms-track-name track)))
          (t (emms-track-simple-description track)))))

(setq emms-track-description-function 'my-emms-track-description)

(global-set-key (kbd "C-c e p") 'emms-playlist-mode-go) ;; Open playlist browser
(global-set-key (kbd "C-c e f") 'emms-play-file)        ;; Play a single file
(global-set-key (kbd "C-c e d") 'emms-add-directory)    ;; Add directory to playlist
(global-set-key (kbd "C-c e s") 'emms-stop)             ;; Stop playback
(global-set-key (kbd "C-c e n") 'emms-next)             ;; Next track
(global-set-key (kbd "C-c e b") 'emms-previous)         ;; Previous track
(global-set-key (kbd "C-c e c") 'emms-show)             ;; Show current track
(global-set-key (kbd "C-c e SPC") 'emms-pause)          ;; Pause/resume

(use-package ready-player
  :ensure t
  :config
  (ready-player-mode +1))
(setq ready-player-my-media-collection-location "~/Music/")

(provide 'plugins/entertainment)
