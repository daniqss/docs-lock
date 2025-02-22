import sys
import json
from github_scraper import GithubScraper
    

def main():
    if len(sys.argv) != 2:
        print("usage: python3 -m main <USERNAME>")
        sys.exit(1)

    username = sys.argv[1]
    repositories = GithubScraper.scrape_repositories(username)

    if repositories is None:
        print('{ "error": "Failed to fetch page or user does not exist" }')
        sys.exit(1)

    for repo in repositories:
        if repo["lang"] == "TypeScript" or repo["lang"] == "JavaScript":
            package_data = GithubScraper.search_for_packagejson(repo["repository"])
            if package_data:
                print(json.dumps(package_data, indent=2))
            

if __name__ == "__main__":
    main()