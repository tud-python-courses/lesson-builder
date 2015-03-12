# Script for adding rendered markdown to a template

Ruby script + template to automatically render a set of markdown pages with the github flavored markdown into a template alongside some meta information defined in build_conf.json.

## Usage
Invoke `ruby build.rb` in the terminal.

Command line options:
- `-d, --working_dir DIRECTORY` set in which directory to look for the build_conf.json and the source files (default '.')
- `-t, --theme_folder DIRECTORY` no use yet
- `-o, --output_dir DIRECTORY` set where to put the rendered html (default <working_dir>/build )
