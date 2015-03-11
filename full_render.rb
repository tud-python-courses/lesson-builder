require 'html/pipeline'
require 'pathname'
require 'json'

include HTML


$pipeline = HTML::Pipeline.new [
    Pipeline::MarkdownFilter,
    Pipeline::TableOfContentsFilter, # add 'name' anchors to all headers and generate toc list
    #Pipeline::CamoFilter,
    #Pipeline::ImageMaxWidthFilter,
    #Pipeline::SyntaxHighlightFilter,
    #Pipeline::EmojiFilter,
    Pipeline::AutolinkFilter
]

$template_file = 'template.html'

default_output_dir = 'build'


def render_one(template, workdir, outdir, config)

    raise "type of workdir" unless workdir.is_a?(Pathname)

    number = config["number"]

    infile_name = config.fetch("source", "lesson_#{number}.md")

    infile = workdir + infile_name

    outfile = Pathname.new "#{outdir}/#{infile_name.rpartition('.')[0]}.html"

    rendered = template % {
        :lesson_number => number,
        :content => render_markdown(number, infile),
        :lesson_title => config.fetch("title", "Python - Kurs #{number}")
    }


    outfile.delete if outfile.exist?

    File.open(outfile, 'w') {|f| f.puts(rendered)}

    [infile, outfile]
end


def render_all(items, workdir, outdir, config_file)

    workdir = Pathname.new workdir

    meta = JSON.parse(File.read(workdir + config_file))

    if items.length == 0
        items = meta.fetch("lessons", [])
    else
        items = meta["lessons"].select do |a|
            items.include?(a["number"]) or items.includes?(a["number"])
        end
    end

    used_template = meta.fetch("template", "use_default")

    if used_template == 'use_default'
        used_template = Pathname.new(__FILE__).parent.join + $template_file
    else
        used_template = workdir + used_template
    end


    template = File.read(used_template)

    template = template % {
        :lesson_number => '%{lesson_number}',
        :content => '%{content}',
        :lesson_title => '%{lesson_title}',
        :stylesheets => meta["theme"].fetch("stylesheets", []).map do |a|
            "<link rel=\"stylesheet\" type=\"text/css\" href=\"#{a}\" media=\"all\">"
        end.join('\n'),
        :scripts => meta["theme"].fetch("scripts", []).map do |a|
            "<script type=\"text/javascript\" src=\"#{a}\">"
        end.join('\n')
    }

    puts template

    items.map do |lesson|
        render_one(template, workdir, outdir, lesson)
    end
end

def render_markdown(lesson_number, file)
    input_file = File.read file

    result = $pipeline.call input_file

    result[:output].to_s
end


def main()
    start_time = Time.new

    workdir, output_dir = ARGV


    # parser.add_argument('--workdir', '-d', default='.', nargs='?', type=str)
    # parser.add_argument('lessons', nargs='*', type=int)
    # parser.add_argument('--theme_folder', '-t', type=str)
    # parser.add_argument('--output_dir', '-o', default=None, type=str)

    config_file = 'build_conf.json'

    rendered = render_all([], workdir, output_dir, config_file)

    files = rendered.map do |elem|
        infile, outfile = elem
        "\n#{infile}  ->  #{outfile}"
    end.join('')

    puts "Rendered #{rendered.length} files in #{Time.new - start_time}: #{files} "
end


if __FILE__ == $0
    main
end
