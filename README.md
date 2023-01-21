# rolling-cache

Adding this to your existing transformer stack automatically manages storing and evicting cached values from a fixed sized cache.

See the example directory for a executable example app.

Before:
```
do
    ... other app things ...
    value <- liftIO . dbQuery $ queryInput
    ... other app things ...
```

After:
```
do
    ... other app things ...
    value <- Cache.fetch (liftIO . dbQuery) queryInput
    ... other app things ...
```
