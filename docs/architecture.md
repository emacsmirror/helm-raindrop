# Architecture

How `helm-raindrop.el` fills its cache file, `helm-raindrop-file`.

With a large collection, a single cache update fans out into hundreds of API requests. They are chained through `request.el` callbacks, throttled by two different kinds of waiting, and written to the file exactly once. This document maps that machinery.

## The collection and page loop

The Raindrop.io API returns at most 50 items per request, so every collection is walked page by page from page 0. A collection is finished when a page comes back with zero items, which costs one extra request per collection.

Items are accumulated in a hidden work buffer (`helm-raindrop--work-buffer-name`). Nothing reaches `helm-raindrop-file` until every collection has been fetched.

```mermaid
flowchart LR
    subgraph col1[collection 123456]
        direction LR
        A0["page 0"] --> A1["page 1"] --> AX["..."] --> AN["page N"] --> AE["page N+1<br/>0 items"]
    end
    subgraph col2[collection 789012]
        direction LR
        B0["page 0"] --> B1["page 1"] --> BX["..."] --> BM["page M"] --> BE["page M+1<br/>0 items"]
    end

    AE ==> B0
    col1 -. "50 items per response" .-> WB
    col2 -. "50 items per response" .-> WB
    WB[("work buffer")] == "written once, after every collection succeeds" ==> FILE["helm-raindrop-file"]

    classDef ok fill:#E3F2F4,stroke:#11707F,color:#0B3A41
    class FILE ok
```

## One request, five outcomes

`helm-raindrop-do-http-request` is the whole loop. It takes a collection ID, a page number and a retry count, and every call ends in one of five ways. Three of them call it again, which is what keeps the loop turning; the dotted edges below are those self-calls.

The two solid end states are the only places where the session stops, and only one of them touches the cache file.

```mermaid
flowchart TD
    HUB["helm-raindrop-do-http-request<br/>collection-id / page / retry-count"]
    HUB --> GATE{"rate limit<br/>remaining = 0?"}

    GATE -->|Yes| WAIT["sleep until reset"]
    WAIT -. "same page again" .-> HUB

    GATE -->|No| REQ["GET /rest/v1/raindrops/:id"]
    REQ --> OK[":success<br/>append items to work buffer"]
    REQ --> NG[":error"]

    OK --> MORE{"any items<br/>in response?"}
    MORE -->|Yes| PAGE["page + 1<br/>retry-count back to 0"]
    PAGE -. "next page" .-> HUB
    MORE -->|No| COLL{"more<br/>collections?"}
    COLL -->|Yes| POP["pop the queue<br/>start at page 0"]
    POP -. "next collection" .-> HUB
    COLL -->|No| FIN["helm-raindrop-session-finish<br/>write helm-raindrop-file"]

    NG --> RETRY{"429 and<br/>retry-count < 3?"}
    RETRY -->|Yes| BACKOFF["sleep retry-after<br/>retry-count + 1"]
    BACKOFF -. "same page again" .-> HUB
    RETRY -->|No| ABORT["helm-raindrop-session-abort<br/>nothing is written"]

    classDef ok fill:#E3F2F4,stroke:#11707F,color:#0B3A41
    classDef stop fill:#FAE9E7,stroke:#A93A31,color:#4A1712
    classDef wait fill:#F7EFDD,stroke:#8E6210,color:#3E2B08
    class FIN ok
    class ABORT stop
    class WAIT,BACKOFF wait
```

## Two kinds of waiting

The rate limit is handled in two places: one runs before a request is sent, the other after a 429 (Too Many Requests) comes back.

The **preemptive gate** reads `x-ratelimit-remaining` from every response. Once it hits 0, the next call stops *before* sending anything and sleeps until `x-ratelimit-reset`. It has no attempt limit, and it is what makes a full update occasionally take minutes rather than seconds.

The **reactive retry** only runs after the server has already answered 429. It sleeps for `retry-after` and tries the same page again, at most three times.

```mermaid
sequenceDiagram
    participant E as helm-raindrop
    participant A as Raindrop.io API

    E->>A: GET page 1
    A-->>E: 200 OK, x-ratelimit-remaining: 0

    Note over E: preemptive gate:<br/>do not send while remaining is 0
    E-)E: sleep until x-ratelimit-reset

    E->>A: GET page 2
    A-->>E: 429, retry-after: 2

    Note over E: reactive retry:<br/>3 attempts at most
    E-)E: sleep retry-after
    E->>A: GET page 2 again
    A-->>E: 200 OK
```

## Failure handling

Any error that is not a retryable 429 ends the session through `helm-raindrop-session-abort`: the collection queue is cleared and the work buffer is discarded without being written. A failed update therefore leaves the previous cache file untouched rather than replacing it with a truncated one.

The abort is always reported, regardless of `helm-raindrop-debug-mode`, because updates normally run from a timer where a silent failure would leave a stale cache unnoticed:

```
[Raindrop] Aborted: failed (error "peculiar") to GET https://api.raindrop.io/... (730.3sec) at 2025-09-26 16:17:32.  /Users/masutaka/.emacs.d/helm-raindrop was not updated.
```

A completed session logs a summary instead, when `helm-raindrop-debug-mode` is `info` or `debug`:

```
Wrote /Users/masutaka/.emacs.d/helm-raindrop
[Raindrop] Total: 235 requests completed for 2 collections (11521 items) in 129.9sec at 2025-09-27 13:04:42.
```

## Where each step lives

| Step | Function |
| --- | --- |
| Entry point, from `M-x` or the timer | `helm-raindrop-http-request` |
| One request and its branches | `helm-raindrop-do-http-request` |
| Rate limit gate | `helm-raindrop-ratelimit-exceeded-p` |
| Sleep until reset | `helm-raindrop-wait-for-ratelimit-reset` |
| "Any items in response?" | `helm-raindrop-next-page-exist-p` |
| Move to the next collection | `helm-raindrop-process-next-collection` |
| "429 and retry-count < 3?" | `helm-raindrop-should-retry-p` |
| Sleep `retry-after` and retry | `helm-raindrop-handle-ratelimit-error` |
| Write the cache file and stop | `helm-raindrop-session-finish` |
| Stop without writing | `helm-raindrop-session-abort` |
