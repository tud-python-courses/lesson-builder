from template import *



def parse_text(markdown, title, subtitle):
    in_quote = False
    set_quote = True
    quote_text = ''
    in_list = False
    list_text = ''
    list_depth = 0
    in_code = False
    set_code = True
    code_text = ''
    code_add_before = ''
    code_add_after = ''
    in_table = False
    tabe_text = ''
    q_l_c_t = [0,0,0,0,0,0,0,0,0,0] #  1==q, 2==l, 3==c, 4==t
    next_line = False


    tex = make_header(title, subtitle)

    for line in markdown:
        print(line)
        if len(line) > 2 and line[:-2] == '\\n':
            line = line[:-2]

        #  Test for Headings
        if (in_quote is False
            and in_list is False
            and in_code is False
            and in_table is False
            ):
            if re_h6.match(line):
                list = catch_text(line, '###### ')
                line = make_section('subsubsection', list[2])
                tex = tex + line
                line = ''
                next_line = False

            if re_h5.match(line):
                list = catch_text(line, '#####')
                line = make_section('subsubsection', list[2])
                tex = tex + line
                line = ''
                next_line = False

            if re_h4.match(line):
                list = catch_text(line, '####')
                line = make_section('subsubsection', list[2])
                tex = tex + line
                line = ''
                next_line = False

            if re_h3.match(line):
                list = catch_text(line, '###')
                line = make_section('subsubsection', list[2])
                tex = tex + line
                line = ''
                next_line = False

            if re_h2.match(line):
                list = catch_text(line, '##')
                line = make_section('subsection', list[2])
                tex = tex + line
                line = ''
                next_line = False

            if re_h1.match(line) :
                if not re_python_comment.match(line):
                    print(re_h1.match(line).start())
                    list = catch_text(line, '#')
                    line = make_section('section', list[2])
                    tex = tex + line
                    line = ''
                    next_line = False

        #  Paragraph
        if re_two_white.match(line):
            if next_line is True:
                line = ''
            else:
                list = catch_text(line, '  ')
                line = list[0] + '\\\\\n'
                next_line = True

        if re_new_line.match(line):
            if next_line is True:
                line = ''
                if in_quote is True:
                    set_quote = False
            else:
                list = catch_text(line, '\\n')
                line = list[0] + '\\\\\n'
                next_line = True

        #  Quote
        if re_quote.match(line) and in_table is False:
            in_quote = True
            list = catch_text(line, '>')
            line = list[2]
            q_l_c_t = move_array(array=q_l_c_t, direction=1, new=1)

        #  Bold
        if re_bold.match(line):
            pat = ''
            if re_bold_under.match(line):
                pat = '__'
            else:
                pat = '**'
            list = catch_text(line, pat, pat)
            line = list[0] + make_bold(list[2]) + list[4]
            q_l_c_t = move_array(array=q_l_c_t, direction=1, new=2)

        #  Italic
        if re_italic.match(line):
            pat = ''
            if re_italic_under.match(line):
                pat = '_'
            else:
                pat = '*'
            list = catch_text(line, pat, pat)
            line = list[0] + make_bold(list[2]) + list[4]
            q_l_c_t = move_array(array=q_l_c_t, direction=1, new=3)

        #  Code
        if re_code_single.match(line) and in_code is False:
            list = catch_text(line, '`', '`')
            line = list[0] + make_code(list[2]) + list[4]

        if re_code_multiple.match(line):
            list = catch_text(line, '```')
            if in_code is True:
                line = list[0]
                code_add_after = list[2]
                set_code = False
            else:
                code_add_before = list[0]
                line = list[2]
                in_code = True


        if in_quote is True and q_l_c_t[0] == 1:
            quote_text = quote_text + line
            in_quote = set_quote
            if set_quote is False:
                tex = tex + make_quote(quote_text)
                move_array(q_l_c_t)
                quote_text = ''
                set_quote = True
        elif in_list is True and q_l_c_t[0] == 2:
            print('')
        elif in_code is True and q_l_c_t[0] == 3:
            code_text = code_text + line
            in_code = set_code
            if set_code is False:
                tex = tex + make_code(code_text) + code_add_after
                code_add_after = ''
                set_code = True
            else:
                tex = tex + code_add_before
                code_add_before = ''
        elif in_table is True and q_l_c_t[0] == 4:
            print('')
        else:
            tex = tex + line

    return tex



def catch_text(line, pattern_first,
               pattern_second='',
               pattern_third='',
               pattern_fourth=''
               ):

    pos_1_s = get_pos(line, pattern_first)
    pos_1_e = pos_1_s + len(pattern_first)
    list = [line[:pos_1_s], line[pos_1_s:pos_1_e],line[pos_1_e:]]
    if pattern_second != '':
        pos_2_s = get_pos(line[pos_1_e:], pattern_second) + pos_1_e
        pos_2_e = pos_2_s + len(pattern_second)
        list = append_line(line, list, pos_1_e, pos_2_s, pos_2_e)
    if pattern_third != '':
        pos_3_s = get_pos(line[pos_2_e:], pattern_third) + pos_2_e
        pos_3_e = pos_3_s + len(pattern_third)
        list = append_line(line, list, pos_2_e, pos_3_s, pos_3_e)
    if pattern_fourth != '':
        pos_4_s = get_pos(line[pos_3_e:], pattern_fourth) + pos_3_e
        pos_4_e = pos_4_s + len(pattern_fourth)
        list = append_line(line, list, pos_3_e, pos_4_s, pos_4_e)
    return list


def move_array(array, direction=0, new=0):
    count = len(array) - 1
    if direction == 1: # go forward
        while count >= 0:
            if count == 0:
                array[count] = new
            else:
                array[count] = array[count - 1]
            count = count - 1
    else:
        i = 0
        while  i <= count:
            if i == count:
                array[i] = 0
            else:
                array[i] = array[i + 1]
            i = i + 1
    return array



def append_line(line, list, pos1, pos2, pos3):
    list.pop()
    list.append(line[pos1:pos2])
    list.append(line[pos2:pos3])
    list.append(line[pos3:])
    return list


def get_pos(line, pattern):
    return line.find(pattern)


def main():
    file = open('{lesson}.md'.format(lesson='lesson_1'), 'r')
    markdown = file.readlines()
    file.close()

    tex_file = open('{lesson}.tex'.format(lesson='lesson_1'), 'w')
    tex = parse_text(markdown, 'Python Kurs', 'Einführung in Python')
    tex_file.write(tex)
    tex_file.close()

if __name__ == '__main__':
    main()
