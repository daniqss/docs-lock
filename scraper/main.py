import sys
import requests
from typing import List, Tuple, Dict
import os
from bs4 import BeautifulSoup # type: ignore
import json

def search_for_repositories_in_html(file_name: str) -> List[Tuple[str, str]]:
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

def scrape_repositories(username: str) -> List[Dict[str, str]]:
    user_repositories: List[Tuple[str, str]] = []

    for i in range(1, 99):
        url = f"https://github.com/{username}?page={i}&tab=repositories"
        response = requests.get(url)
        
        if response.status_code == 200:
            file_name = f"{username}_repos.html"
            with open(file_name, "w", encoding="utf-8") as file:
                file.write(response.text)
            print(f"{username} repositories page {i} downloaded successfully")

            repositories_info = search_for_repositories_in_html(file_name)
            
            os.remove(file_name)

            if len(repositories_info) == 0:
                break
            else:
                user_repositories.extend(repositories_info)
        else:
            print(f"Failed to download {username} repositories page")
            break
    
    return [{"repository": repo, "lang": lang} for repo, lang in user_repositories]

def main():
    if len(sys.argv) != 2:
        print("usage: python3 -m main <USERNAME>")
        sys.exit(1)
    
    username = sys.argv[1]
    repositories = scrape_repositories(username)
    
    print(json.dumps(repositories, indent=2))

if __name__ == "__main__":
    main()