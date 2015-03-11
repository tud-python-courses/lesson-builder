import re


#  LaTeX-Templates
def make_header(title, subtitle):
    header = '''\\documentclass[]{tudbeamer}
%\\documentclass[]{beamer}
\\usepackage[T1]{fontenc}
\\usepackage{graphicx}
\\usepackage{listings}
\\usepackage{verbatim}
\\usepackage{menukeys}
\\usepackage{ngerman}
\\usepackage{listings}

\\definecolor{codegray}{gray}{0.97}
\\newcommand{\codeblock}[1]{\colorbox{codegray}{\\texttt{#1}}}


\\title{''' + '{title}'.format(title=title) + '''}
\\subtitle{''' + '{subtitle}'.format(subtitle=subtitle) + '''}

\\begin{document}

\\maketitle
% -------------------------- Table of Contens ---------------------------------

\\begin{frame}
\\tableofcontents'''
    return header

def make_footer():
    footer = '''\\end{frame}
\\end{document}'''
    return footer

def make_section():
    section = '''\\end{frame}

% -------------------------- ''' + '{name}'.format(name=name) + ''' ---------------------------------
\\''' + '{sectiontype}'.format(sectiontype=sectiontype) + '{' + '{name}'.format(name=name) + '''}
\\beginopen_curlyframeclose_curly
\\frametitle{''' + '{name}'.format(name=name) + '}'



#  Patterns

#  Headings
re_h6 = re.compile('######.*')
re_h5 = re.compile('#####.*')
re_h4 = re.compile('####.*')
re_h3 = re.compile('###.*')
re_h2 = re.compile('##.*')
re_h1 = re.compile('#.*')

#  Paragraph
re_two_white = re.compile('.*  ')
re_new_line = re.compile('.*\\n')

#  Quotes
re_quote = re.compile('>.*')

#  Styling
re_bold = re.compile('.*((__.*__)|(\\*\\*.*\\*\\*)).*')
re_italic = re.compile('.*(( _.*_ )|(\\*.*\\*)).*')

#  List
re_list_unsorted = re.compile('[ ]*((\\*)|(-)) .*')
re_list_sorted = re.compile('[ ]*[\d]+ .*')

# Code
re_code_single = re.compile('.*\\`.*\\`.*')
re_code_multiple = re.compile('\\`\\`\\`.*')

#  Links
re_link = re.compile('.*\\[.*\\]\\(.*\\).*')

#  Strikethrough
re_strikethrough = re.compile('.*~~.*~~.*')

# Table
re_table = re.compile('\\|?[.*\\|]+.*')
re_table_head = re.compile('\\|?[\\:\\-\\|]+')
