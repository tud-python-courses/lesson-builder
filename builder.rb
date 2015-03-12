require 'pathname'
require 'set'



module BuildTools

  VALID_LATEX_COMMANDS = Set.new %w(htlatex pdflatex)

  module Compile

    def self.latex_to_html(source_file)
      latex source_file, command: 'htlatex'
    end

    def self.latex(source_file, command: 'pdflatex')
      raise "invalid latex command #{command}" unless VALID_LATEX_COMMANDS.include? command
      # TODO check command is valid
      `#{command} #{source_file}`
    end

  end

  module Build

    def self.build(source_file, source_dir, target_dir, command)

      source_dir = Pathname source_dir

      cdir = Pathname Dir.getwd

      output_file_name = construct_output_file_name source_file

      Dir.chdir source_dir

      latex source_file, command: command

      Dir.chdir cdir

      FileUtils.cp(source_dir + output_file_name, target_dir)
    end

    def self.batch_build(source_files, source_dir, target_dir, command)

      source_dir = Pathname source_dir

      cdir = Pathname Dir.getwd

      Dir.chdir source_dir

      compiled_files = source_files.map do |file|
        latex file, command: command

        construct_output_file_name(file)
      end

      Dir.chdir cdir

      source_files.zip(
        compiled_files.map do |file|
          FileUtils.cp(source_dir + file, target_dir)
          target_dir + file
        end
      )
    end

    def self.from_config(config, source_dir: nil, target_dir: nil)

      source_dir = config['source_dir'] if source_dir.nil?
      source_dir = Pathname source_dir

      target_dir = config.fetch('target_dir', source_dir + 'build') if target_dir.nil?
      target_dir = Pathname target_dir
      pdf_command = config.fetch 'pdf_latex_command', 'pdflatex'
      html_command = config.fetch 'html_latex_command', 'htlatex'

      lessons = config['lessons']

      # filter for lessons that will be html built
      html_built = lessons.select do |lesson|
        lesson.fetch 'html', false
      end

      # filter for lessons that will be pdf built
      pdf_built = lessons.select do |lesson|
        lesson.fetch 'pdf', false
      end

      # get the file names
      html_built = html_built.map do |lesson|
        lesson.fetch 'source', "lesson_#{file}.tex"
      end

      pdf_built = pdf_built.map do |lesson|
        lesson.fetch 'source', "lesson_#{file}.tex"
      end

      # build html
      html_res = batch_build html_built, source_dir, target_dir, html_command

      # build pdf
      pdf_res = batch_build pdf_built, source_dir, target_dir, pdf_command

      { :html => html_res, :pdf => pdf_res }

    end

    def self.build_directory(source, target)
      source = Pathname source

      conf = JSON.parse File.read(source + CONFIG_NAME)

      BuildTools::Build.from_config conf, source_dir: source, target_dir: target
    end

    def self.build_dir_with_output(source, target)
      start_time = Time.new

      built = build_directory source, target

      puts "Built #{built[:html].length + built[:pdf].length} files in #{Time.new - start_time}"

      built.each do |key, list|
        list.each do |original, compiled|
          puts "#{original}  ->  #{compiled}"
        end
      end
    end

    private

    def construct_output_file_name(source_file)

      base = File.basename source_file, '.*'

      base + '.html'
    end

  end

end