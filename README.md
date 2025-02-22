# Docs Lock


## Usage
### Frontend
```bash
cd client
npm install
npm run dev

# generate frontend types
npm run types
```

### Backend

- Compile the server project
```bash
make OBJ=server TARGET=compile
```

- Up/Down mongo container
```bash
cd server
make mongo_up
make mongo_down
```

- Init server and open shell
```bash
make OBJ=server TARGET=shell or 
cd server && make shell
```

- Checks and run tests
```bash
make OBJ=server TARGET=check
make OBJ=server TARGET=test
```

### Scraper
```bash
cd scraper
make install
make run
```
