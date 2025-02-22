import sys
import requests
from typing import List
import os
from bs4 import BeautifulSoup # type: ignore

def search_for_repositories(file_name: str) -> List[str]:
    repositories = []
    
    with open(file_name, "r", encoding="utf-8") as file:
        html_content = file.read()
    
    soup = BeautifulSoup(html_content, 'html.parser')
    
    repo_elements = soup.find_all(class_='wb-break-all')
    
    for repo in repo_elements:
        repo_name = repo.find('a')
        if repo_name:
            repositories.append(repo_name.text.strip())
    
    return repositories

def download_repositories_page(username: str) -> List[str]:
    user_repositores: List[str] = []

    for i in range(1, 99):
        url = f"https://github.com/{username}?page={i}&tab=repositories"
        response = requests.get(url)
        
        if response.status_code == 200:
            file_name = f"{username}_repos.html"
            with open(file_name, "w", encoding="utf-8") as file:
                file.write(response.text)
            print(f"{username} repositories page {i} downloaded successfully")

            repositories_names = search_for_repositories(file_name)
            
            os.remove(file_name)

            if len(repositories_names) == 0:
                return user_repositores
            else:
                user_repositores.extend(repositories_names)
        else:
            print(f"Failed to download {username} repositories page")
    return ""

def main():
    if len(sys.argv) != 2:
        print("usage: python3 -m main <USERNAME>")
        sys.exit(1)
    
    username = sys.argv[1]
    repositories = download_repositories_page(username)
    print(repositories)

if __name__ == "__main__":
    main()