# Meat API

## What is this?

A sample of my ability to construct a REST API. This one exposes data concerning Meat Bars (from https://epicbar.com/) and who has eaten them. It uses Web.Scotty to serve the API, Cassava to import CSVs, and Persistent/Esqueleto to persist and query the data with SQLite.

## Building the Project

``` sh
stack install // really, that's it!
./meatbar // to run the built project
```

## Testing the Project
``` sh
stack test
```

## Using the Project
The project's routes are located at localhost:3000/{route}. The SQLite database created by the application is located at sqlite.db, in the root of the project's directory structure.