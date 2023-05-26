(add-to-list 'my:consult-gh-default-orgs-list (string-trim (url-filename (url-generic-parse-url my:maingithub)) "\/" "\/"))

(setq my:consult-gh-default-orgs-list (append my:consult-gh-default-orgs-list (remove "" (split-string (my:consult-gh--call-process "org" "list") "\n"))))

(setq my:consult-gh--default-maxnum 100)

(add-to-list 'savehist-additional-variables 'my:consult-gh--known-orgs-list)
(add-to-list 'savehist-additional-variables 'my:consult-gh--known-repos-list)
