# servant-elm-auth0

Servant template for a SPA in Elm with authentication through Auth0

## Building

Note: requires <https://github.com/alasconnect/auth0> as a peer (../auth0)

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

Thanks to Andrew Newman (@newmana) for inspiration and support.

Auth0 api calling code handily provided by [alsconnect](https://github.com/alasconnect/auth0>)
