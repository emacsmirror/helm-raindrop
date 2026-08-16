# CLAUDE.md

`helm-raindrop.el` is a single-file Emacs Lisp package, distributed via MELPA, that gives Helm an interface to [Raindrop.io](https://raindrop.io/). It fetches items from the API into a cache file on a timer, and Helm searches that file rather than the API.

## Source

@helm-raindrop.el

## Architecture

The fetch loop is harder to follow than its size suggests: hundreds of chained `request.el` callbacks, two different ways of waiting on the rate limit, and a single write at the very end. Update this doc whenever that flow changes.

@docs/architecture.md

## Verifying a change

There is no test suite. Byte compile and confirm it finishes with no warnings:

```
emacs -Q -batch --eval '(package-initialize)' -f batch-byte-compile helm-raindrop.el
```

Delete the generated `helm-raindrop.elc` afterwards. It is not gitignored, so it will otherwise show up as an untracked file.

## Conventions

- Backward compatibility is not a concern
- Docs come in English and Japanese pairs: `README.md` / `README_ja.md`, and `docs/architecture.md` / `docs/architecture_ja.md`. Change both together
- The package version lives in the `;; Version:` header of `helm-raindrop.el`
