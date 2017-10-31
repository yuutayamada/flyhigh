# Flyhigh -- On the FLY HIGHlight package.

This package is still work in progress; it may have breaking change(s)
without notice.

Currently I'm targeting nim-mode's (Nim language's major-mode).

## installation

Probably most easiest way to use this package is using el-get.

``` elisp
;; el-get-sources
(:name flyhigh
      :type github
      :depends (dash deferred) ; might need other package(s)
      :pkgname "yuutayamada/flyhigh"
      :minimum-emacs-version "26")
```
## TODO -- Design goal

- [ ] this package should not block user input; do tasks asynchronously
      (partially done using deferred.el)
- [X] example (living example is flyhigh-nimsuggest)
- [ ] trigger highlight by
  - [X] (1) when the cursor is moved to outside of invisible window
    - [ ] this just needs to refresh highlight inside of visible
          window
       - [ ] provide dedicated idle timer variable (this is
             different usage against (2))
    - [ ] if there are uncompleted highlight from previous query,
          we should use cached highlight and don't make new query.
  - [X] (2) after change (`after-change-functions` hook)
    - [ ] provide dedicated idle timer variable for this, but this
          timer should be longer than other packages' idle timer like
          flycheck, and company
  - [ ]
- [X] be highlighted from visible window
- [ ] highlight invisible overlays
- [ ] keep used overlay(s) until:
  - [ ] buffer change
    - [ ] but removal of overlay(s) should be from visible overlays
      - [ ] and should be kept invisible overlays until it become visible
      - [ ] but during idle time, incorrect invisible overlays should
            be fixed asynchronously
  - [ ]
- [ ] stable APIs
- [ ] register MELPA (but currently this package is too immature)
- [ ] make some tests using faceup.el
- [ ] document
