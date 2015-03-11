import re


#  LaTeX-Templates
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


\\title{Python-Kurs}
\\subtitle{Einführung in Python}

\\begin{document}

\\maketitle
% -------------------------- Table of Contens ---------------------------------

\\begin{frame}
\\tableofcontents'''


footer = '''\\end{frame}
\\end{document}'''


section = '''\\endopen_curlyframeclose_curly

% -------------------------- {name} ---------------------------------
\\{section}open_curly{name}close_curly
\\beginopen_curlyframeclose_curly
\\frametitleopen_curly{name}close_curly'''

#  Regex
re_section = re.compile('^<h1>.*</h1>$')
re_subsection = re.compile('^<h2>.*</h2>$')
re_subsubsection = re.compile('^<h3>.*</h3>$')
re_line = re.compile('^<p>.*</p>$')
re_itemize_start = re.compile('^<ul>$')
re_itemize_end = re.compile('^</ul>$')
re_item = re.compile('^<li>.*</li>$')
re_bd = re.compile('^<strong>.*</strong>$')
re_link = re.compile('^<a href=".*">.*</a>$')
re_code = re.compile('^<code>.*</code>$')
