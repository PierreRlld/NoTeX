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
        self.Checkbox.selection_color = term.green

def folder_question(name,type,question,path):
    subfolders = [ f.name for f in os.scandir(path) if f.is_dir() ]
    if subfolders == []:
        return None
    else:
        q_class = getattr(inquirer, type)
        q_class(name=name, message=question, choices=subfolders)

def main():
  questions = [
      inquirer.Checkbox(
        name="kill_list", message="Who you want to kill?", choices=["a","b","c"], default=["a","c"]
    )
  ]
  answers = inquirer.prompt(questions, theme=CustomTheme(), raise_keyboard_interrupt=True, answers=None)
  print(answers["kill_list"]) 

if __name__ == "__main__":
    main()