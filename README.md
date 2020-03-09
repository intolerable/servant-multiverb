# servant-multiverb

The existing Servant `Verb` combinator has no way to dynamically choose a HTTP response status code at run-time. This package introduces a `Verbs` combinator which can contain multiple `Result` types, each with their own status code.

The response code is determined by different arms of nested `Either`s, as follows:

```
type ExampleAPI =
  Verbs 'GET '[JSON] (Result 503 String <|> Result 200 Int)

-- Server ExampleAPI ~ Handler (Either String Int)
-- :\/ is also included, so this is possible
exampleHandler :: Handler (String :\/ Int)
exampleHandler = do
  i :: Int <- randomIO
  if odd i
    then pure $ Left "random number was odd!"
    else pure $ Right i
```

There is currently no `servant-client` support for this combinator.
