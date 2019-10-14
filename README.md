# servant-elm-auth0

Servant template for a SPA in Elm with authentication through Auth0

## Building

Note: requires <https://github.com/state-machine-io/auth0> as a peer (../auth0)

### Servant

This project uses stack:

```
stack build
```


### Elm

Just run yarn to set up.

```
yarn install
```

## Running

Uses stack exec and [parcel-js](https://parceljs.org/) under the hood.

```
./bin/server <configFile>
```

## Thanks

Thanks to [Andrew Newman](<https://github.com/newmana>) for inspiration and support.

The authentication code is loosely based off the examples at <https://github.com/haskell-servant/servant-auth>.

Auth0 api calling code handily provided by [alsconnect](https://github.com/alasconnect/auth0).
