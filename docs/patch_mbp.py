import re

with open('graph_testing_research.tex', 'r') as f:
    c = f.read()

# 1. documentclass
c = c.replace(
    r'\documentclass[12pt, a4paper, twoside]{article}',
    r'\documentclass[10pt, oneside]{article}'
)

# 2. lstfiracode
c = c.replace(r'\usepackage{lstfiracode}', r'')

# 3. geometry
old_geo = '\\usepackage[\n  a4paper,\n  top=25mm,\n  bottom=25mm,\n  left=30mm,\n  right=25mm,\n  headheight=25pt\n]{geometry}'
new_geo = '\\usepackage[\n  paperwidth=185mm,\n  paperheight=118mm,\n  top=7mm,\n  bottom=7mm,\n  left=9mm,\n  right=9mm,\n  headheight=0pt,\n  headsep=0pt,\n  footskip=4mm\n]{geometry}'
c = c.replace(old_geo, new_geo)
print("geo:", "OK" if new_geo in c else "FAIL")

# 4. header
old_hdr = '\\pagestyle{fancy}\n\\fancyhf{}\n\\fancyhead[LE]{\\small\\color{MidGray}\\textit{Графовое тестирование}}\n\\fancyhead[RO]{\\small\\color{MidGray}\\textit{development\\_system}}\n\\fancyfoot[C]{\\small\\color{MidGray}\\thepage}\n\\renewcommand{\\headrulewidth}{0.4pt}\n\\renewcommand{\\headrule}{\\hbox to\\headwidth{\\color{RuleColor}\\leaders\\hrule height \\headrulewidth\\hfill}}\n\\renewcommand{\\footrulewidth}{0pt}'
new_hdr = '\\pagestyle{fancy}\n\\fancyhf{}\n\\fancyfoot[C]{\\fontsize{7}{8}\\selectfont\\color{MidGray}\\thepage}\n\\renewcommand{\\headrulewidth}{0pt}\n\\renewcommand{\\footrulewidth}{0pt}'
if old_hdr in c:
    c = c.replace(old_hdr, new_hdr)
    print("hdr: OK")
else:
    # try to find pagestyle
    idx = c.find('\\pagestyle{fancy}')
    print(f"hdr: NOT FOUND, pagestyle at {idx}")
    print(repr(c[idx:idx+300]))

# 5. section sizes
c = c.replace(
    '  {\\large\\bfseries\\color{AccentBlue}}\n  {\\thesection.}',
    '  {\\normalsize\\bfseries\\color{AccentBlue}}\n  {\\thesection.}'
)
c = c.replace(
    '  {\\normalsize\\bfseries\\color{DarkGray}}\n  {\\thesubsection.}',
    '  {\\small\\bfseries\\color{DarkGray}}\n  {\\thesubsection.}'
)

# 6. spacing
c = c.replace('\\titlespacing*{\\section}{0pt}{18pt}{8pt}', '\\titlespacing*{\\section}{0pt}{9pt}{4pt}')
c = c.replace('\\titlespacing*{\\subsection}{0pt}{12pt}{4pt}', '\\titlespacing*{\\subsection}{0pt}{6pt}{2pt}')
c = c.replace('\\titlespacing*{\\subsubsection}{0pt}{8pt}{2pt}', '\\titlespacing*{\\subsubsection}{0pt}{4pt}{1pt}')
c = c.replace('\\setlength{\\parskip}{6pt}', '\\setlength{\\parskip}{2pt}')
c = c.replace('\\onehalfspacing', '\\setstretch{1.15}')

# 7. code font
c = c.replace(
    'basicstyle=\\small\\ttfamily\\color{DarkGray},',
    'basicstyle=\\fontsize{7}{8.5}\\ttfamily\\color{DarkGray},'
)

# 8. titlepage
old_tp = '\\begin{titlepage}\n  \\newgeometry{margin=30mm}'
new_tp = '\\begin{titlepage}'
c = c.replace(old_tp, new_tp)

# Find and replace LARGE title block
old_header_block = '  \\vspace*{10mm}\n\n  % Верхняя линия\n  {\\color{AccentBlue}\\rule{\\textwidth}{1.5pt}}\n  \\vspace{6mm}\n\n  % Метка\n  {\\sffamily\\small\\color{MidGray}{ТЕХНИЧЕСКОЕ ИССЛЕДОВАНИЕ}}\\\\[4mm]\n\n  % Главный заголовок\n  {\\sffamily\\LARGE\\bfseries\\color{AccentBlue}\n    Графовое тестирование:\\\\[4pt]\n    автогенерация тестов из описания системы\\\\[4pt]\n    в \\texttt{development\\_system}\n  }\n\n  \\vspace{8mm}\n  {\\color{AccentBlue}\\rule{\\textwidth}{0.4pt}}\n  \\vspace{8mm}'

new_header_block = '  \\vspace*{3mm}\n\n  {\\color{AccentBlue}\\rule{\\textwidth}{1.2pt}}\n  \\vspace{3mm}\n\n  {\\sffamily\\fontsize{6.5}{8}\\color{MidGray} ТЕХНИЧЕСКОЕ ИССЛЕДОВАНИЕ}\\\\[3mm]\n\n  {\\sffamily\\fontsize{11}{14}\\bfseries\\color{AccentBlue}\n    Графовое тестирование:\\\\[2pt]\n    автогенерация тестов из описания системы\n  }\\\\[2mm]\n  {\\sffamily\\fontsize{8.5}{10}\\color{DarkGray}\\texttt{development\\_system}}\n\n  \\vspace{4mm}\n  {\\color{AccentBlue}\\rule{\\textwidth}{0.4pt}}\n  \\vspace{4mm}'

if old_header_block in c:
    c = c.replace(old_header_block, new_header_block)
    print("title header block: OK")
else:
    print("title header block: NOT FOUND (will try regex)")
    # Use regex to find and replace the title section
    pattern = r'\\vspace\*\{10mm\}.*?\\vspace\{8mm\}'
    match = re.search(pattern, c, re.DOTALL)
    if match:
        c = c[:match.start()] + '  \\vspace*{3mm}\n\n  {\\color{AccentBlue}\\rule{\\textwidth}{1.2pt}}\n  \\vspace{3mm}\n\n  {\\sffamily\\fontsize{6.5}{8}\\color{MidGray} ТЕХНИЧЕСКОЕ ИССЛЕДОВАНИЕ}\\\\[3mm]\n\n  {\\sffamily\\fontsize{11}{14}\\bfseries\\color{AccentBlue}\n    Графовое тестирование:\\\\[2pt]\n    автогенерация тестов из описания системы\n  }\\\\[2mm]\n  {\\sffamily\\fontsize{8.5}{10}\\color{DarkGray}\\texttt{development\\_system}}\n\n  \\vspace{4mm}\n  {\\color{AccentBlue}\\rule{\\textwidth}{0.4pt}}\n  \\vspace{4mm}' + c[match.end():]
        print("title header block: regex OK")

# Replace big minipage annotation with compact version
old_ann = '  % Аннотация на титульной\n  \\begin{minipage}{0.92\\textwidth}\n    \\itshape\\color{DarkGray}\n    Данное исследование описывает методологию автоматической генерации\n    тестов на основе графа состояний системы. Человек описывает систему\n    как граф (текстом, JSON, или Lisp-DSL), а специализированный поток\n    \\textbf{граф-тестировщик} автоматически порождает тест-кейсы:\n    для целевых состояний --- тесты достижимости, для нежелательных ---\n    тесты на исключения. Исследование охватывает теоретические основы\n    (Model-Based Testing, Property-Based Testing, Reachability Analysis),\n    конкретную архитектуру в рамках существующей метациркулярной системы,\n    примеры кода на Common Lisp и пошаговый план реализации.\n  \\end{minipage}'
new_ann = '  {\\fontsize{7.5}{10}\\itshape\\color{DarkGray}\n    Человек описывает систему как граф (текст, JSON, Lisp-DSL),\n    поток \\textbf{граф-тестировщик} порождает тест-кейсы:\n    для целевых состояний~--- достижимость,\n    для нежелательных~--- исключения.\n  }'
if old_ann in c:
    c = c.replace(old_ann, new_ann)
    print("annotation: OK")
else:
    print("annotation: NOT FOUND")

# Replace meta-table
old_meta = '  % Мета-таблица\n  \\begin{tabular}{ll}\n    \\textbf{Версия:}  & 1.0 \\\\[2pt]\n    \\textbf{Дата:}    & 14 марта 2026 \\\\[2pt]\n    \\textbf{Проект:}  & \\texttt{development\\_system} \\\\[2pt]\n    \\textbf{Стек:}    & Common Lisp (SBCL), OpenRouter, Telegram \\\\[2pt]\n    \\textbf{Предназначение:} & автоматизация тестирования метациркулярной среды \\\\\n  \\end{tabular}'
new_meta = '  \\vspace{3mm}\n  \\begin{tabular}{@{}ll@{}}\n    {\\fontsize{6.5}{7.5}\\color{MidGray}Версия:} & {\\fontsize{6.5}{7.5} 1.0 · 14 марта 2026} \\\\[1pt]\n    {\\fontsize{6.5}{7.5}\\color{MidGray}Стек:}   & {\\fontsize{6.5}{7.5} Common Lisp, SBCL, OpenRouter} \\\\\n  \\end{tabular}'
if old_meta in c:
    c = c.replace(old_meta, new_meta)
    print("meta-table: OK")
else:
    print("meta-table: NOT FOUND")

# 9. restoregeometry
c = c.replace('\\restoregeometry', '')

# 10. tableofcontents
c = c.replace('\\tableofcontents\n', '{\\fontsize{8}{10}\\selectfont\\tableofcontents}\n')

with open('graph_testing_mbp.tex', 'w') as f:
    f.write(c)
print(f"DONE: {len(c)} chars, {c.count(chr(10))} lines")
