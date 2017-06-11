* my dotfiles
They are deployed via `stow` by the script `deploy.sh`.

* Other dotfile/settings not covered by the files here:

** High res support in Firefox
in the file `~/.mozilla/firefox/d21eg0iw.default/prefs.js`:
```
[...]
user_pref("layout.css.devPixelsPerPx", "1.3");
[...]
```
