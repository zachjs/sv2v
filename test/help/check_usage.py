import subprocess

with open("../../README.md", "r") as file:
    text_readme = file.read()

text_help = subprocess.check_output(["../../bin/sv2v", "--help"]).decode()
text_usage = "```\n" + "\n".join(text_help.split("\n")[2:-5]) + "\n```"

if text_usage not in text_readme:
    raise RuntimeError(f"'{text_usage}' not found in '{text_readme}'")
