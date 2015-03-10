import re
import pathlib
import subprocess
import os
import json
import sys
import argparse


build_markdown_ruby_code = """
require 'github/markdown'

File.write('{tempfile}', GitHub::Markdown.render(File.read('{file}')))
"""

md_builder_file = 'md_compile.rb'

template_file = 'template.html'

config_file = 'lessons.json'

default_output_dir = 'build'


def _render(workdir, filename, lesson_number, lesson_title):
    rendered_markdown = render_markdown(lesson_number, filename)
    
    with open(template_file) as file:
        template = file.read()

    return template.format(
        lesson_number=lesson_number,
        content=rendered_markdown,
        lesson_title=lesson_title,
    )


def render_markdown(lesson_number, file):
    return subprocess.check_output(('ruby', md_builder_file, file)).decode('utf-8')


def render_one(workdir, outdir, number, config):

    infile = config.get('source', 'lesson_{}.md'.format(number))

    infile = pathlib.Path(workdir) / infile

    outfile = '{}/{}.html'.format(outdir, infile.stem)

    rendered = _render(
        workdir,
        str(infile),
        number,
        config.get('title', 'Python - Kurs {}'.format(number))
    )

    if os.path.exists(outfile):
        os.remove(outfile)
    print(rendered, file=open(outfile, mode='w+'))


def main():
    parser = argparse.ArgumentParser()

    parser.add_argument('--workdir', '-d', default='.', nargs='?', type=str)
    parser.add_argument('lessons', nargs='*', type=int)
    parser.add_argument('--theme_folder', '-t', type=str)
    parser.add_argument('--output_dir', '-o', default=None, type=str)

    args = parser.parse_args()

    workdir = args.workdir

    if args.output_dir is None:
        output_dir = str(pathlib.Path(workdir) / default_output_dir)
    else:
        output_dir = args.output_dir

    if not os.path.exists(output_dir):
        print('Output folder {} created'.format(outdir))
        os.mkdir(output_dir)

    meta = json.load(open(str(pathlib.Path(workdir) / config_file)))

    if len(args.lessons) == 0:
        for number, single_conf in meta.items():
            render_one(workdir, output_dir, number, single_conf)

    else:
        for number in args.lessons:
            print(type(number))
            number = int(number)

            try:
                conf = meta.get(str(number))
                render_one(workdir, output_dir, number, conf)
            except KeyError:
                print('Lesson {} does not exist'.format(number))



if __name__ == '__main__':
    main()
