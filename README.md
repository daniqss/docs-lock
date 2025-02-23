# DOCS.LOCK

# Dependencies
erlang
rebar3
docker and docker-compose
make
nodejs
python

# How to run
## frontend
```bash
cd client
npm install
npm run dev

# generate frontend types
npm run types
```

## server
```bash
cd server
rebar3 shell
```

## database
```bash
cd server
docker-compose up -d
```

### Features  

- A web-based multi-user platform designed for developers.  
- Allows users to search for information and ask questions about various skills, programming languages, and competencies.  
- Supports a community-driven or enterprise environment, enabling teams to share knowledge efficiently.  
- Includes both a frontend and a backend to provide a seamless user experience.  
- Supports real-time interaction and dynamic content updates.  
- Automatically enriches the database by dynamically adding new skills and competencies.  
- Uses a Python script to analyze GitHub repositories of newly registered users, detecting libraries and programming languages to expand the skillset database.  

### Usage  

To use the platform, simply access the web application through your browser. Register or log in to start searching for information, asking questions, and interacting with the community. If enabled, newly registered users with linked GitHub accounts will contribute to the platform’s knowledge base through automated repository analysis.

## License
