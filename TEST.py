import re
import inquirer
from inquirer.themes import Default
from inquirer import List, Checkbox
from blessed import Terminal
term = Terminal()
import os

class CustomTheme(Default):
    def __init__(self):
        super().__init__()
        self.List.selection_color = term.green

def folder_question(type,question,path):
    subfolders = [ f.name for f in os.scandir(path) if f.is_dir() ]
    if subfolders == []:
        return None
    else:
        q_class = getattr(inquirer, type)
        return q_class()

def main(dropbox,git):
    dx_folders = [ f.name for f in os.scandir(dropbox) if f.is_dir() ]
    questions = [
    inquirer.List('dropbox_folder',
                    message="Quel dossier à update",
                    choices=dx_folders,
                    carousel=True
                )
    ]
    answers = inquirer.prompt(questions, theme=CustomTheme(), raise_keyboard_interrupt=True)


if __name__ == "__main__":
    main(dropbox="/Users/prld/Desktop/TEST", 
         git="/Users/prld/git/NoTeX/")