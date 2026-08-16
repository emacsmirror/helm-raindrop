# helm-raindrop.el

[![melpa badge][melpa-badge]][melpa-link]
[![melpa stable badge][melpa-stable-badge]][melpa-stable-link]
[![Ask DeepWiki][deepwiki-badge]][deepwiki-link]

[melpa-link]: https://melpa.org/#/helm-raindrop
[melpa-stable-link]: https://stable.melpa.org/#/helm-raindrop
[deepwiki-link]: https://deepwiki.com/masutaka/emacs-helm-raindrop
[melpa-badge]: https://melpa.org/packages/helm-raindrop-badge.svg
[melpa-stable-badge]: https://stable.melpa.org/packages/helm-raindrop-badge.svg
[deepwiki-badge]: https://deepwiki.com/badge.svg

<p>
  <a href="./README.md"><img alt="README in English" src="https://img.shields.io/badge/English-d9d9d9"></a>
  <a href="./README_ja.md"><img alt="日本語のREADME" src="https://img.shields.io/badge/日本語-d9d9d9"></a>
</p>

## Introduction

`helm-raindrop.el` は [Raindrop.io](https://raindrop.io/) の Helm インターフェースです。

- Helm の絞り込みで Raindrop.io のアイテムを検索できます
- 複数の collection と、その入れ子に対応しています
- 各アイテムのタイトル、URL、タグ、ノート、ハイライトを表示します
- Migemo による日本語検索に対応しています
- キャッシュは非同期で定期的に更新されます

## Demo

https://github.com/user-attachments/assets/291b762c-4a8c-4721-a188-db8900619d6c

## Requirements

- Emacs 29.1 以上
- Helm 4.0.4 以上
- request.el 0.3.2 以上

## Installation

[MELPA](https://melpa.org) から package.el でインストールできます（`M-x package-install helm-raindrop`）。

## Setup

### 基本の設定（collection が 1 つの場合）

```lisp
(setq helm-raindrop-access-token "Your app test token")
(setq helm-raindrop-collection-ids "123456") ;; https://app.raindrop.io/my/123456 なら 123456
(helm-raindrop-initialize)
```

### collection が複数の場合

```lisp
(setq helm-raindrop-access-token "Your app test token")
(setq helm-raindrop-collection-ids '("123456" "789012" "345678")) ;; 複数の collection ID
(helm-raindrop-initialize)
```

helm-raindrop.el が使うのはテストトークンです。アクセストークンは必要ありません。[公式ドキュメント](https://developer.raindrop.io/v1/authentication/token)に次の記載があるためです。

> [!NOTE]
> If you just want to test your application, or do not plan to access any data except yours account you don't need to make all of those steps.
>
> Just go to [App Management Console](https://app.raindrop.io/settings/integrations) and open your application settings. Copy Test token and use it as described in [Make authorized calls](https://developer.raindrop.io/v1/authentication/calls).

## Usage

### `helm-raindrop`

Raindrop.io のアイテムを検索するコマンドです。

```
M-x helm-raindrop
```

#### アクション

- **Browse URL**（デフォルト）: 選択したアイテムをブラウザで開く
- **Copy ITEM**: アイテムの情報を Markdown 形式でクリップボードにコピーする
- **Show ITEM**: アイテムの情報を Markdown 形式で専用バッファに表示する

#### キーバインド

- `RET`: デフォルトのアクションを実行する（Browse URL）
- `TAB`: アクションの一覧を表示する
- `C-j`: Helm を閉じずにプレビューする

### `helm-raindrop-source`

Helm ソースとして公開しているので、自分の Helm 設定に組み込んだり、他のソースと組み合わせたりできます。

```lisp
;; 単独のソースとして使う
(helm :sources 'helm-raindrop-source
      :buffer "*helm raindrop*")

;; 他のソースと組み合わせる
(helm :sources '(helm-raindrop-source
                 helm-source-bookmarks
                 helm-source-recentf)
      :buffer "*helm multi*")
```

## Customization

### 必須の設定

- `helm-raindrop-access-token`（デフォルト: `nil`）
    - Raindrop.io のテストトークン
    - https://app.raindrop.io/settings/integrations で作成できます
    - 必須の設定です
- `helm-raindrop-collection-ids`（デフォルト: `nil`）
    - アイテムを取得する collection の ID
    - collection の URL が `https://app.raindrop.io/my/123456` なら `"123456"` を指定します
    - 文字列 1 つ、または複数の collection を指定する文字列のリストを渡せます
    - 特別な値:
        - `"0"`: 全アイテム（ゴミ箱を除く）
        - `"-1"`: 未整理のアイテム
        - `"-99"`: ゴミ箱のアイテム
    - 例:
        - `"123456"`（collection 1 つ）
        - `'("123456" "789012")`（collection 複数）
    - 必須の設定です

### 任意の設定

- `helm-raindrop-include-nested-collections`（デフォルト: `t`）
    - 入れ子になった collection のアイテムも取得するかどうか
- `helm-raindrop-file`（デフォルト: `~/.emacs.d/helm-raindrop`）
    - Raindrop.io のアイテムを保存するキャッシュファイルのパス
    - このファイルは一定間隔で自動的に更新されます
- `helm-raindrop-interval`（デフォルト: `10800` = 3 時間）
    - キャッシュを自動更新する間隔（秒）
    - Raindrop.io API からアイテムを取得し直します
- `helm-raindrop-debug-mode`（デフォルト: `nil`）
    - API リクエストのログ出力レベル
    - 指定できる値:
      - `nil`: 出力しない
      - `'info`: サマリーのみ
      - `'debug`: すべてのメッセージ

## Architecture

キャッシュファイルができるまでの流れは [docs/architecture_ja.md](./docs/architecture_ja.md) にまとめています。collection と page のループ、2 種類の rate limit の待ち方、更新に失敗したときにキャッシュがどうなるかを扱っています。
