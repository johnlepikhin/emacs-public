
(global-set-key (kbd "M-s r") 
            (lambda () 
              (interactive)
              (ispell-change-dictionary "russian" nil)
	      (flyspell-buffer)))

(global-set-key (kbd "M-s e") 
            (lambda () 
              (interactive) 
              (ispell-change-dictionary "american" nil)
	      (flyspell-buffer)))

(global-set-key (kbd "M-s <right>")
		(lambda ()
		  (interactive)
		  (flyspell-goto-next-error)))

(ispell-change-dictionary "russian" nil)

(flyspell-mode)

;; you can also use "M-x ispell-word" or hotkey "M-$". It pop up a multiple choice
;; @see http://frequal.com/Perspectives/EmacsTip03-FlyspellAutoCorrectWord.html
(global-set-key (kbd "S-<return>") 'flyspell-auto-correct-word)

(provide 'init-spelling)
