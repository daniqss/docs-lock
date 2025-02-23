# DOCS.LOCK

## Dependencies
erlang
rebar3
docker and docker-compose
make
nodejs
python

## How to run
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

- Up/Down mongodb container
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

## Features  

- A web-based multi-user platform designed for developers.  
- Allows users to search for information and ask questions about various skills, programming languages, and competencies.  
- Supports a community-driven or enterprise environment, enabling teams to share knowledge efficiently.  
- Includes both a frontend and a backend to provide a seamless user experience.  
- Supports real-time interaction and dynamic content updates.  
- Automatically enriches the database by dynamically adding new skills and competencies.  
- Uses a Python script to analyze GitHub repositories of newly registered users, detecting libraries and programming languages to expand the skillset database.  

## Usage  

To use the platform, simply access the web application through your browser. Register or log in to start searching for information, asking questions, and interacting with the community. If enabled, newly registered users with linked GitHub accounts will contribute to the platform’s knowledge base through automated repository analysis.

## License
