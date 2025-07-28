# helm-raindrop.el

[![melpa badge][melpa-badge]][melpa-link]
[![melpa stable badge][melpa-stable-badge]][melpa-stable-link]

[melpa-link]: https://melpa.org/#/helm-raindrop
[melpa-stable-link]: https://stable.melpa.org/#/helm-raindrop
[melpa-badge]: https://melpa.org/packages/helm-raindrop-badge.svg
[melpa-stable-badge]: https://stable.melpa.org/packages/helm-raindrop-badge.svg

## Introduction

`helm-raindrop.el` provides a Helm interface for [Raindrop.io](https://raindrop.io/).

You can browse and search your Raindrop.io items using Helm interface.

## Requirements

- Emacs 29.1 or higher
- Helm 4.0.4 or higher
- request.el 0.3.2 or higher

## Installation

You can install `helm-raindrop.el` from [MELPA](https://melpa.org) with package.el (`M-x package-install helm-raindrop`).

## Setup

### Basic setup (single collection)

```lisp
(setq helm-raindrop-access-token "Your app test token")
(setq helm-raindrop-collection-ids "123456") ;; For https://app.raindrop.io/my/123456, it's 123456.
(helm-raindrop-initialize)
```

### Multiple collections setup

```lisp
(setq helm-raindrop-access-token "Your app test token")
(setq helm-raindrop-collection-ids '("123456" "789012" "345678")) ;; Multiple collection IDs
(helm-raindrop-initialize)
```

helm-raindrop.el is designed to use a test token instead of an access token, based on the reasons outlined in the [official documentation](https://developer.raindrop.io/v1/authentication/token):

> [!NOTE]
> If you just want to test your application, or do not plan to access any data except yours account you don't need to make all of those steps.
>
> Just go to [App Management Console](https://app.raindrop.io/settings/integrations) and open your application settings. Copy Test token and use it as described in [Make authorized calls](https://developer.raindrop.io/v1/authentication/calls).

## Usage

### `helm-raindrop`

Interactive command to search and browse your Raindrop.io items.

```
M-x helm-raindrop
```

This command opens a Helm interface where you can:
- Search through your items using fuzzy matching
- Browse items with tags displayed in brackets (e.g., `[tag1][tag2]`)
- Perform various actions on selected items

#### Available Actions

- **Browse URL** (default): Open the selected item in your web browser
- **Show URL**: Display the item URL in the minibuffer
- **Show NOTE**: Display the item's note in the minibuffer

#### Key Bindings

- `RET`: Execute the default action (Browse URL)
- `TAB`: Show available actions
- `C-j`: Show persistent action (preview without closing Helm)

### `helm-raindrop-source`

A Helm source that can be used in your own Helm configurations.

```lisp
(helm :sources 'helm-raindrop-source
      :buffer "*helm raindrop*")
```

This source provides:
- Multi-line display support
- Migemo support for Japanese search
- Customizable actions

## Customization

### Required Settings

- `helm-raindrop-access-token` (default: `nil`)
    - Your Raindrop.io test token
    - You can create one at https://app.raindrop.io/settings/integrations
    - This is a required setting
- `helm-raindrop-collection-ids` (default: `nil`)
    - Collection IDs to fetch items from
    - If the collection URL is `https://app.raindrop.io/my/123456`, then use `"123456"`
    - Can be either a single string or a list of strings for multiple collections
    - Special values:
        - `"0"`: All items (except trash)
        - `"-1"`: Unsorted items
        - `"-99"`: Trash items
    - Examples:
        - `"123456"` (single collection)
        - `'("123456" "789012")` (multiple collections)
    - This is a required setting

### Optional Settings

- `helm-raindrop-include-nested-collections` (default: `t`)
    - Whether to include items from nested collections
- `helm-raindrop-file` (default: `~/.emacs.d/helm-raindrop`)
    - Cache file path for storing Raindrop items
    - This file is automatically updated at specified intervals
- `helm-raindrop-interval` (default: `10800` = 3 hours)
    - Number of seconds between automatic cache updates
    - The cache is refreshed by fetching items from Raindrop.io API
- `helm-raindrop-debug-mode` (default: `nil`)
    - Debug logging level for API requests
    - Available options:
      - `nil`: No logging
      - `'info`: Summary only
      - `'debug`: All messages
