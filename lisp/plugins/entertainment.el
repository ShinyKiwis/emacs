;; Entertainment-related configuration
(use-package emms
  :ensure t
  :config
  (require 'emms-setup)
  (emms-all)
  (emms-mode-line-disable)
  (emms-default-players)
  (setq emms-source-file-default-directory "~/Music/"))

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
