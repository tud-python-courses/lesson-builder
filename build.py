import re
import pathlib
import subprocess
import os
import json
import sys


build_markdown_ruby_code = """
require 'github/markdown'

File.write('{tempfile}', GitHub::Markdown.render(File.read('{file}')))
"""

template_file = 'template.html'

config_file = 'lessons.json'


def _render(filename, lesson_number, lesson_title):
    tmpfile = filename + '.tmp'

    if os.path.exists(tmpfile):
        os.remove(tmpfile)

    render_markdown(lesson_number, filename, tmpfile)

    with open(template_file) as file:

        with open(tmpfile) as tmpcontent:
            content = tmpcontent.read()

        os.remove(tmpfile)

        template = file.read()
        return template.format(
            lesson_number=lesson_number,
            content=content,
            lesson_title=lesson_title,
        )





def render_markdown(lesson_number, file, tmpfile):
    code = build_markdown_ruby_code.format(
        file=file,
        tempfile=tmpfile
    )

    ruby_tmp_file_name = 'render_lesson_{}.rb'.format(lesson_number)

    if os.path.exists(ruby_tmp_file_name):
        os.remove(ruby_tmp_file_name)

    print(code, file=open(ruby_tmp_file_name, mode='w+'))

    subprocess.call(('ruby', ruby_tmp_file_name))

    os.remove(ruby_tmp_file_name)


def render_one(number, config):

    infile = config.get('source', 'lesson_{}.md'.format(number))

    infile = pathlib.Path(infile)

    outfile = 'build/{}.html'.format(infile.stem)

    rendered = _render(
        str(infile),
        number,
        config.get('title', 'Python - Kurs {}'.format(number))
    )

    if os.path.exists(outfile):
        os.remove(outfile)
    print(rendered, file=open(outfile, mode='w+'))


def main():
    meta = json.load(open(config_file))

    if len(sys.argv) == 1 or sys.argv[1].lower() == 'all':
        for number, single_conf in meta.items():
            render_one(number, single_conf)

    elif len(sys.argv) ==2 and sys.argv[1].isdigit():
        number = int(sys.argv[1])

        try:
            conf = meta.get(str(number))
            render_one(number, conf)
        except KeyError:
            print('Lesson {} does not exist'.format(number))

    else:
        print('Arguments invalid, expected none or one of type int')


if __name__ == '__main__':
    main()
