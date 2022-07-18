# Project Title

A simple todo app in haskell using PlanetScale as mysql db.

## Description

Commands list:

1. `--list`
2. `--insert {todoitem}`

## Getting Started

### Dependencies

- Signup on [Planetscale](https://planetscale.com/) and create a db with table as **todos**
- Click on 'connect' and download and store a .env file with the necessary fields
- Setup [pscale](https://docs.planetscale.com/concepts/planetscale-environment-setup) and run the below command to connect to the db

```
$ pscale connect {dbname} main --port 3000
```

- Verify the setup by connecting using the mysql binary

### Installing

```
$ git clone https://github.com/InfiniteVerma/planetodo
$ cd planetodo
$ cabal install
```

### Executing program

- List todo items: `cabal run planetodo -- --list`
- Insert new item: `cabal run planetodo -- --insert hello world`
