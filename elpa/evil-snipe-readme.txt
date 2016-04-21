Snipe is an attempt to bring a marriage of vim-sneak and vim-seek to evil-mode.

Specifically, it brings two functions to evil: skulking and sniping. Skulking
pertains to finding and jumping to two-character matches. Sniping pertains to
performing actions (yank, delete, change, etc.) on remote words, far from the
cursor.

Skulking is synonymous with f/F and t/T. By default (like vim-seek and f/F/t/T),
it only search for matches on the same line relative to (point). If you prefer
buffer-wide search, see evil-snipe-scope.

Sniping, however, is like vim-seek's remote and presential leaps. For instance,
you can delete a nearby word that contains "ev" with direv. That's d for delete,
ir for inner-remote and ev for 'word that contains ev.

See the README.md for more information.
