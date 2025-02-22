import sys
import requests
from typing import List, Tuple, Dict
import os
from bs4 import BeautifulSoup # type: ignore
import json

def search_for_repositories_in_html(file_name: str) -> List[Tuple[str, str]] | None:
    repositories = []
    
    with open(file_name, "r", encoding="utf-8") as file:
        html_content = file.read()
    
    soup = BeautifulSoup(html_content, 'html.parser')
    
    repo_containers = soup.find_all('div', {'class': 'col-10 col-lg-9 d-inline-block'})
    
    for container in repo_containers:
        repo_name_element = container.find(class_='wb-break-all')
        if not repo_name_element or not repo_name_element.find('a'):
            continue
            
        repo_name = repo_name_element.find('a').text.strip()
        
        language_element = container.find('span', {'itemprop': 'programmingLanguage'})
        language = language_element.text.strip() if language_element else "No language specified"
        
        repositories.append((repo_name, language))
    
    return repositories

def scrape_repositories(username: str) -> List[Dict[str, str]] | None:
    user_repositories: List[Tuple[str, str]] = []
    found_repos = False  # Flag to track if any repositories were found

    for i in range(1, 99):
        url = f"https://github.com/{username}?page={i}&tab=repositories"
        response = requests.get(url)

        if response.status_code == 200:
            file_name = f"{username}_repos.html"
            with open(file_name, "w", encoding="utf-8") as file:
                file.write(response.text)

            repositories_info = search_for_repositories_in_html(file_name)
            os.remove(file_name)

            if repositories_info:
                user_repositories.extend(repositories_info)
                found_repos = True
            elif not found_repos and i == 1:
                return None

            if len(repositories_info) == 0 and found_repos:
                break
        elif response.status_code == 404:
            return None
        else:
            return None

    if not found_repos and not user_repositories:
        return None

    return [{"repository": f"https://github.com/{username}/{repo}", "lang": lang} for repo, lang in user_repositories]

def search_for_packagejson(repo_url: str):
    package_json_url = f"{repo_url.replace('github.com', 'raw.githubusercontent.com')}/main/package.json"

    try:
        response = requests.get(package_json_url)
        response.raise_for_status()
        try:
            return response.json()
        except json.JSONDecodeError:
            return None
    except requests.exceptions.RequestException:
        return None

    

def main():
    if len(sys.argv) != 2:
        print("usage: python3 -m main <USERNAME>")
        sys.exit(1)

    username = sys.argv[1]
    repositories = scrape_repositories(username)

    if repositories is None:
        print('{ "error": "Failed to fetch page or user does not exist" }')
        sys.exit(1)

    for repo in repositories:
        if repo["lang"] == "TypeScript" or repo["lang"] == "JavaScript":
            package_data = search_for_packagejson(repo["repository"])
            if package_data:
                print(json.dumps(package_data, indent=2))
            

if __name__ == "__main__":
    main()