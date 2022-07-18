# Planetodo

A simple :) todo app in haskell using PlanetScale as mysql db.

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

### Screenshots

After adding three items: 

mysql query              |  cabal run planetodo -- --list
:-------------------------:|:-------------------------:
![image](https://user-images.githubusercontent.com/45547198/179513164-17e3fff6-40c6-4579-b64e-1ec3077fb82b.png)  |![image](https://user-images.githubusercontent.com/45547198/179513623-c6ace77b-e263-4607-9088-bb4546bbc6b9.png)
