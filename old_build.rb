require 'html/pipeline'
require 'pathname'
require 'json'
require 'optparse'


include HTML


$pipeline = Pipeline.new [
    Pipeline::MarkdownFilter,
    Pipeline::TableOfContentsFilter, # add 'name' anchors to all headers and generate toc list
    #Pipeline::CamoFilter,
    #Pipeline::ImageMaxWidthFilter,
    #Pipeline::SyntaxHighlightFilter,
    #Pipeline::EmojiFilter,
    Pipeline::AutolinkFilter
]

$template_file = 'template.html'

$default_output_dir = 'build'


def render_one(template, workdir, outdir, config)

    raise 'type of workdir' unless workdir.is_a?(Pathname)

    number = config['number']

    infile_name = config.fetch('source', "lesson_#{number}.md")

    infile = workdir + infile_name

    outfile = Pathname.new "#{outdir}/#{infile_name.rpartition('.')[0]}.html"

    rendered = template % {
        :lesson_number => number,
        :content => render_markdown(infile),
        :lesson_title => config.fetch('title', "Python - Kurs #{number}")
    }


    outfile.delete if outfile.exist?

    File.open(outfile, 'w') {|f| f.puts(rendered)}

    [infile, outfile]
end


def render_all(items, workdir, outdir, config_file)

    meta = JSON.parse(File.read(workdir + config_file))

    if items.length == 0
        items = meta.fetch('lessons', [])
    else
        items = meta.fetch('lessons', []).select do |a|
            items.include?(a['number'])
        end
    end

    used_template = meta['template']

    if used_template == nil or used_template == 'use_default'
        used_template = Pathname.new(__FILE__).parent.join + $template_file
    else
        used_template = workdir + used_template
    end


    template = File.read(used_template)

    static_vars = meta.fetch('static_template_variables', {}).map do |key, value|
        [key.to_sym, value]
    end

    vars = {
        :lesson_number => '%{lesson_number}',
        :content => '%{content}',
        :lesson_title => '%{lesson_title}',
        :stylesheets => meta['theme'].fetch('stylesheets', []).map do |a|
            "<link rel=\"stylesheet\" type=\"text/css\" href=\"#{a}\" media=\"all\">"
        end.join('\n'),
        :scripts => meta['theme'].fetch('scripts', []).map do |a|
            "<script type=\"text/javascript\" src=\"#{a}\">"
        end.join('\n')
    }.merge(
        static_vars
    )

    template = template % vars

    items.map do |lesson|
        render_one(template, workdir, outdir, lesson)
    end
end

def render_markdown(file)
    input_file = File.read file

    result = $pipeline.call input_file

    result[:output].to_s
end


def main
    start_time = Time.new

    options = {}

    OptionParser.new do |opts|
        opts.on('-d', '--working_dir DIRECTORY', 'Set working directory') do |n|
            options[:workdir] = n
        end

        opts.on('-t', '--theme_folder', 'specify theme folder to use') do |n|
            options[:theme_folder] = n
        end

        opts.on('-o', '--output_dir' 'Specify the output directory') do |n|
            options[:output_dir] = n
        end
    end.parse!

    lessons = ARGV.map {|n| n.to_i}

    # parser.add_argument('--workdir', '-d', default='.', nargs='?', type=str)
    # parser.add_argument('lessons', nargs='*', type=int)
    # parser.add_argument('--theme_folder', '-t', type=str)
    # parser.add_argument('--output_dir', '-o', default=None, type=str)

    config_file = 'build_conf.json'

    workdir = Pathname.new options.fetch(:workdir, '.')
    output_dir = options.fetch(:output_dir, workdir + $default_output_dir)

    rendered = render_all(lessons, workdir, output_dir, config_file)

    files = rendered.map do |infile, outfile|
        "\n#{infile}  ->  #{outfile}"
    end.join('')

    puts "Rendered #{rendered.length} files in #{Time.new - start_time}: #{files} "
end


if __FILE__ == $0
    main
end
