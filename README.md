# Eta Servant Example

This is an proof-of-concept example of using the [Servant](https://haskell-servant.github.io) web framework in Eta. 

NOTE: The dependencies `wai-servlet`(https://github.com/jneira/wai-servlet) is still under development (although nearing completion) and this example should actually depend on [wai-servlet-handler-jetty](https://github.com/jneira/wai-servlet-handler-jetty) which is also under development.

# Instructions

1. Download & install [wai-servlet](https://github.com/jneira/wai-servlet).

```
git clone https://github.com/jneira/wai-servlet
cd wai-servlet
etlas install
```

2. Download & run this example.

```
git clone https://github.com/rahulmutt/eta-servant-example
cd eta-servant-example
etlas install --dependencies-only
etlas run
```

3. Go to `http://localhost:9000` and see the generated docs on how to play with the API.

4. Read the [Servant Tutorial](http://haskell-servant.readthedocs.io/en/stable/) and modify this example to your heart's content!
