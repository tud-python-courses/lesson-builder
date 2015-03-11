from .template import *


def parse_line(line):
    break_line = False
    line = line.replace('\\', '\\textbackslash')
    line = line.replace('#', '\\#')
    line = line.replace('_', '\\char`_')
    line = line.replace('&gt;', '$>$')
    line = line.replace('&lt;', '$<$')
    line = line.replace('&amp;', '&')
    line = line.replace('&#39;', "'")

    for i in range(10):
        # Sections
        if re_section.match(line):
            section_name = catch_text(line, 'h1')
            line = section.format(section='section', name=section_name)

        if re_subsection.match(line):
            section_name = catch_text(line, 'h2')
            line = section.format(section='subsection', name=section_name)

        if re_subsubsection.match(line):
            section_name = catch_text(line, 'h3')
            line = section.format(section='subsubsection', name=section_name)

        # Text
        if re_line.match(line):
            line = re_line.sub(catch_text(line, 'p'), line)
            break_line = True

        if re_bd.match(line):
            replace = '\\textbf{' + catch_text(line, 'strong') + '}'
            line = re_bd.sub(replace, line)

        if re_link.match(line):
            link = '\\href{' + catch_text(line, 'a') + '}'
            line = re_link.sub(link, line)
            
        # Itemize
        if re_itemize_start.match(line):
            line = '\\begin{itemize}'

        if re_itemize_end.match(line):
            line = '\\end{itemize}'

        if re_item.match(line):
            item_content = '\\item ' + catch_text(line, 'li')
            line = re_item.sub(item_content, line)



        # Code
        if re_code.match(line):
            code = '\\codeblock{' + catch_text(line, 'code') + '}'
            line = re_code.sub(code, line)

    line = line
    line = line.replace('open_curly', '{')
    line = line.replace('close_curly', '}')
    if break_line is True:
        line = line + '\\\\'
    line = line + '\n'
    return line


def catch_text(line, pattern):
    end = line.find ('</{}>'.format(pattern))
    if pattern == 'a':
        start = line.find('<{}>'.format(pattern)) + len(pattern + ' href="') + 2
        mid_end = line.find('">')
        mid_start = mid_end + 2
        return line[start:mid_end] + '}{' + line[mid_start:end]
    else:
        start = line.find('<{}>'.format(pattern)) + len(pattern) + 2
    return line[start:end]


def maketex(input_text):
    tex = header
    for line in input_text:
        tex = tex + parse_line(line)
    tex = tex + footer
    output = open('output.tex', 'w')
    output.write(tex)
    output.close
