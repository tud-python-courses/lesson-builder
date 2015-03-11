from .template import *



def parse_text(input, title, subtitle):
    in_quote = False
    next_line = False
    list_depth = 0
    in_code = False
    in_table = False

    tex = make_header(title, subtitle)

    for line in input:

        #  Test for Headings
        if re_h6.match(line):
            list = catch_text(line, '######')
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

        if re_h1.match(line):
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
            else:
                list = catch_text(line, '\\n')
                line = list[0] + '\\\\\n'
                next_line = True




def catch_text(line, pattern_first,
               pattern_second='',
               pattern_third='',
               pattern_fourth=''
               ):

    pos_1_s = get_pos(pattern_first)
    pos_1_e = pos_f_s + len(pattern_first)
    list = [line[:pos_1_s], line[pos_1_s:pos_1_e],line[pos_1_e:]]
    if patters_second != '':
        pos_2_s = get_pos(pattern_second)
        pos_2_e = pos_s_s + len(pattern_second)
        list = append_list(line, list, pos_1_e, pos_2_s, pos_2_e)
    if pattern_third != '':
        pos_3_s = get_pos(pattern_third)
        pos_3_e = pos_t_s + len(pattern_third)
        list = append_list(line, list, pos_2_e, pos_3_s, pos_3_e)
    if pattern_fourth != '':
        pos_4_s = get_pos(pattern_fourth)
        pos_4_e = pos_t_s + len(pattern_fourth)
        list = append_list(line, list, pos_3_e, pos_4_s, pos_4_e)
    return list


def replace_mdargs(list):
    list_leght = len(list)





    return list

def append_line(line, list, pos1, pos2, pos3):
    list.pop()
    list.append(line[pos1:pos2])
    list.append(line[pos2:pos3])
    list.append(line[pos3:])
    return list


def get_pos(line, pattern)
    return line.find(pattern)
