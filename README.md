# Eta Servant Example

This is an proof-of-concept example of using the [Servant](https://haskell-servant.github.io) web framework in Eta. 

# Prerequisites

- Eta 0.0.6b5+

# Instructions

1. Install the dependencies and run.

```
etlas install --dependencies-only
etlas run
```

NOTE: If the installation step seems to hang with no console output, try:

```
etlas install --dependencies-only --allow-newer
etlas run
```

which should make it go faster, but may make your build fail in rare cases.

2. Go to `http://localhost:9000` and see the generated docs on how to play with the API.

3. Read the [Servant Tutorial](http://haskell-servant.readthedocs.io/en/stable/) and modify this example to your heart's content!
