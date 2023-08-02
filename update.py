import os
import re
import inquirer
from inquirer.themes import Default
from blessed import Terminal
term = Terminal()
from repo_check_size import *
from os.path import basename
import zipfile
from shutil import copytree, rmtree
from tqdm import tqdm

base_dropbox = "/Users/prld/Dropbox"
base_git = "/Users/prld/git/NoTeX"

def retrieve_file_paths(dirName):
  filePaths = []
  for root, directories, files in os.walk(dirName):
    for filename in files:
        filePath = os.path.join(root, filename)
        if '/._' in filePath:
          pass
        else:
          filePaths.append(filePath)
  return filePaths

class CustomTheme(Default):
    def __init__(self):
        super().__init__() 
        self.List.selection_cursor = "➤"
        self.Checkbox.selection_icon = "➤"
        self.List.selection_color = term.green
        self.Checkbox.selection_color = term.green
        self.Checkbox.selected_color = term.green
        self.Checkbox.unselected_icon = "*"

def main(dropbox):
    dx_folders = [ f.name for f in os.scandir(dropbox) if f.is_dir() ]
    q1 = [
    inquirer.List(name='dropbox_folder',
                    message="Quel dossier à maj",
                    choices=dx_folders,
                    carousel=True,
                )
    ]
    answers1 = inquirer.prompt(q1, theme=CustomTheme(), raise_keyboard_interrupt=True)["dropbox_folder"]
    subfolders = [ f.name for f in os.scandir(dropbox+"/"+answers1) if f.is_dir() ]

    if len(subfolders) == 0:
        print(term.darkred("Pas de dossier à maj dans "+answers1))
        return("quit")
    else:
        q2 = [
            inquirer.Checkbox(name='selected_subfolder',
                        message="Choisir dossiers à maaj",
                        choices=subfolders,
                    )
        ]
        answers2 = inquirer.prompt(q2, theme=CustomTheme(), raise_keyboard_interrupt=True)["selected_subfolder"]
        return(answers1,answers2)

def pool_paste(dropbox, git, base_folder, selected_folders):

    with tqdm(total=len(selected_folders)) as pbar:
        for folder_name in selected_folders:
            copytree(dropbox+"/"+base_folder+"/"+folder_name,
                    git+"/"+base_folder+"/"+folder_name,
                    dirs_exist_ok=True)
            pbar.update(1)

if __name__ == "__main__":
    print('-------------------------------------------------')
    size = check_repo_size("https://github.com/PierreRlld/NoTeX")
    print(">>> Repo. Git plein à",str(round(100*(size/1000/1000),1))+"%")
    main_res = main(dropbox="/Users/prld/Desktop/TEST")
    print(main_res)
    if main_res == "quit":
        quit()
    else:
        pool_paste(dropbox="/Users/prld/Desktop/TEST",
                   git="/Users/prld/Desktop/TEST_git",
                   base_folder = main_res[0],
                   selected_folders = main_res[1])