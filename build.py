import re
import pathlib
import subprocess
import os
import json
import sys
import argparse
from datetime import datetime


md_builder_file = str(pathlib.Path(__file__).parent / 'md_compile.rb')

template_file = 'template.html'

config_file = 'build_conf.json'

default_output_dir = 'build'

stylesheet_format = '<link rel="stylesheet" type="text/css" href="{}" media="all">'

script_format = '<script type="text/javascript" src="{}">'


def render_markdown(lesson_number, file):
    try:
        return subprocess.check_output(('ruby', md_builder_file, file)).decode('utf-8')
    except subprocess.CalledProcessError as e:
        print(e.output)
        raise


def render_one(template, workdir, outdir, config):

    assert isinstance(workdir, pathlib.Path)

    number = config['number']

    infile = config.get('source', 'lesson_{}.md'.format(number))

    outfile = '{}/{}.html'.format(outdir, infile.rsplit('.', 1)[0])

    rendered = template.format(
        lesson_number=number,
        content=render_markdown(number, str(workdir / infile)),
        lesson_title=config.get('title', 'Python - Kurs {}'.format(number))
    )

    if os.path.exists(outfile):
        os.remove(outfile)

    print(rendered, file=open(outfile, mode='w+'))

    return infile, outfile


def render_all(items, workdir, outdir, config_file):

    workdir = pathlib.Path(workdir)

    meta = json.load(open(str(workdir / config_file)))

    if len(items) == 0:
        items = meta.get('lessons')
    else:
        items = filter(lambda a: a['number'] in items or int(a['number']) in items, meta['lessons'])

    used_template = meta.get('template', 'use_default')

    if used_template == 'use_default':
        used_template = str(pathlib.Path(__file__).parent / template_file)
    else:
        used_template = str(workdir / used_template)

    with open(used_template) as file:
        template = file.read()

    template = template.format(
        lesson_number='{lesson_number}',
        content='{content}',
        lesson_title='{lesson_title}',
        stylesheets='\n'.join(
            stylesheet_format.format(a) for a in meta['theme'].get('stylesheets', ())
        ),
        scripts='\n'.join(
            script_format.format(a) for a in meta['theme'].get('scripts', ())
        ),
        **meta.get('static_template_variables', {})
    )

    for lesson in items:
        yield render_one(template, workdir, outdir, lesson)


def format_time(t):
    return '{} seconds'.format(t.days * 24 * 60 * 60 + t.seconds)


def main():
    start_time = datetime.now()

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

    rendered = tuple(render_all(args.lessons, workdir, output_dir, config_file))

    print(
        'Rendered {} files in {}: {}'.format(
            len(rendered),
            format_time(datetime.now() - start_time),
            '\n'.join('\n{} -> {}'.format(infile, outfile) for infile, outfile in rendered)
        )
    )




if __name__ == '__main__':
    main()
