# Architecture

<p>
  <a href="./architecture.md"><img alt="Architecture in English" src="https://img.shields.io/badge/English-d9d9d9"></a>
  <a href="./architecture_ja.md"><img alt="日本語のアーキテクチャドキュメント" src="https://img.shields.io/badge/日本語-d9d9d9"></a>
</p>

`helm-raindrop.el` がキャッシュファイル `helm-raindrop-file` を作るまでの仕組みです。

アイテムが多いと、キャッシュを 1 回更新するだけで API リクエストは数百回に及びます。`request.el` のコールバックが次のリクエストを呼ぶ形で連鎖し、2 種類の待ち方で間隔が空き、最後にちょうど 1 回だけファイルに書き込みます。

## collection と page の二重ループ

Raindrop.io API は 1 リクエストにつき最大 50 件しか返しません。そのため collection ごとに page 0 から順にたどります。0 件のページが返ってきた時点でその collection は終わりとみなすので、collection 1 つにつき空振りのリクエストが 1 回入ります。

取得したアイテムは隠しバッファ（`helm-raindrop--work-buffer-name`）に溜め込みます。すべての collection を取り終えるまで `helm-raindrop-file` には何も書きません。

```mermaid
flowchart LR
    subgraph col1[collection 123456]
        direction LR
        A0["page 0"] --> A1["page 1"] --> AX["..."] --> AN["page N"] --> AE["page N+1<br/>0 件"]
    end
    subgraph col2[collection 789012]
        direction LR
        B0["page 0"] --> B1["page 1"] --> BX["..."] --> BM["page M"] --> BE["page M+1<br/>0 件"]
    end

    AE ==> B0
    col1 -. "1 レスポンスあたり 50 件" .-> WB
    col2 -. "1 レスポンスあたり 50 件" .-> WB
    WB[("work buffer")] == "全 collection の成功後に 1 回だけ書く" ==> FILE["helm-raindrop-file"]

    classDef ok fill:#E3F2F4,stroke:#11707F,color:#0B3A41
    class FILE ok
```

## 1 リクエストの 5 つの終わり方

ループの全体が `helm-raindrop-do-http-request` に収まっています。引数は collection ID、page 番号、リトライ回数の 3 つで、1 回の呼び出しは必ず次の 5 つのどれかで終わります。そのうち 3 つは自分自身を呼び直し、この 3 つがループを回します。下の図の点線がその自己呼び出しにあたります。

実線の終点は 2 つだけで、そこでセッションが止まります。キャッシュファイルに触れるのは片方だけです。

```mermaid
flowchart TD
    HUB["helm-raindrop-do-http-request<br/>collection-id / page / retry-count"]
    HUB --> GATE{"rate limit の<br/>残数が 0 ?"}

    GATE -->|Yes| WAIT["reset までスリープ"]
    WAIT -. "同じ page をやり直す" .-> HUB

    GATE -->|No| REQ["GET /rest/v1/raindrops/:id"]
    REQ --> OK[":success<br/>items を work buffer へ追記"]
    REQ --> NG[":error"]

    OK --> MORE{"レスポンスに<br/>items がある ?"}
    MORE -->|Yes| PAGE["page + 1<br/>retry-count は 0 に戻す"]
    PAGE -. "次の page" .-> HUB
    MORE -->|No| COLL{"次の<br/>collection がある ?"}
    COLL -->|Yes| POP["キューを 1 つ進めて<br/>page 0 から"]
    POP -. "次の collection" .-> HUB
    COLL -->|No| FIN["helm-raindrop-session-finish<br/>helm-raindrop-file に書く"]

    NG --> RETRY{"429 かつ<br/>retry-count < 3 ?"}
    RETRY -->|Yes| BACKOFF["retry-after だけスリープ<br/>retry-count + 1"]
    BACKOFF -. "同じ page をやり直す" .-> HUB
    RETRY -->|No| ABORT["helm-raindrop-session-abort<br/>何も書かない"]

    classDef ok fill:#E3F2F4,stroke:#11707F,color:#0B3A41
    classDef stop fill:#FAE9E7,stroke:#A93A31,color:#4A1712
    classDef wait fill:#F7EFDD,stroke:#8E6210,color:#3E2B08
    class FIN ok
    class ABORT stop
    class WAIT,BACKOFF wait
```

## 2 種類の待ち方

rate limit は 2 か所で扱っています。片方はリクエストを送る前に、もう片方は 429（Too Many Requests）が返ってきた後に動きます。

「事前の待機」では、すべてのレスポンスから `x-ratelimit-remaining` を読み取ります。これが 0 になると、次の呼び出しはリクエストを送る手前で止まり、`x-ratelimit-reset` までスリープします。回数の上限はありません。キャッシュの更新に数分かかることがあるのは、これが理由です。

「事後のリトライ」が動くのは、サーバーが 429 を返してきた後です。`retry-after` だけスリープして同じ page をやり直し、多くても 3 回でやめます。

```mermaid
sequenceDiagram
    participant E as helm-raindrop
    participant A as Raindrop.io API

    E->>A: GET page 1
    A-->>E: 200 OK, x-ratelimit-remaining: 0

    Note over E: 事前の待機:<br/>残数が 0 の間は送らない
    E-)E: x-ratelimit-reset までスリープ

    E->>A: GET page 2
    A-->>E: 429, retry-after: 2

    Note over E: 事後のリトライ:<br/>多くても 3 回
    E-)E: retry-after だけスリープ
    E->>A: GET page 2 をやり直す
    A-->>E: 200 OK
```

## エラーの扱い

リトライ対象の 429 以外のエラーが起きると、`helm-raindrop-session-abort` を通ってセッションを終えます。collection のキューを空にし、work buffer は書き出さずに捨てます。更新に失敗したときは、前回のキャッシュファイルがそのまま残ります。途中までの内容で上書きすることはありません。

中断したことは、`helm-raindrop-debug-mode` の設定にかかわらず必ず message に出力します。通常はタイマーから更新が走るため、黙って失敗するとキャッシュが古いまま気づけないからです。

```
[Raindrop] Aborted: failed (error "peculiar") to GET https://api.raindrop.io/... (730.3sec) at 2025-09-26 16:17:32.  /Users/masutaka/.emacs.d/helm-raindrop was not updated.
```

正常に終わった場合は、`helm-raindrop-debug-mode` が `info` か `debug` のときにサマリーを出力します。

```
Wrote /Users/masutaka/.emacs.d/helm-raindrop
[Raindrop] Total: 235 requests completed for 2 collections (11521 items) in 129.9sec at 2025-09-27 13:04:42.
```

## 各ステップの実装場所

| ステップ | 関数 |
| --- | --- |
| 起動（`M-x` またはタイマー） | `helm-raindrop-http-request` |
| 1 リクエストとその分岐 | `helm-raindrop-do-http-request` |
| rate limit の残数チェック | `helm-raindrop-ratelimit-exceeded-p` |
| reset までスリープ | `helm-raindrop-wait-for-ratelimit-reset` |
| 「レスポンスに items がある ?」 | `helm-raindrop-next-page-exist-p` |
| 次の collection へ進む | `helm-raindrop-process-next-collection` |
| 「429 かつ retry-count < 3 ?」 | `helm-raindrop-should-retry-p` |
| `retry-after` だけスリープしてリトライ | `helm-raindrop-handle-ratelimit-error` |
| キャッシュファイルを書いて終了 | `helm-raindrop-session-finish` |
| 書かずに終了 | `helm-raindrop-session-abort` |
