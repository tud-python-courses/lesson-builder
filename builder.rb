require 'pathname'
require 'set'


$valid_latex_commands = Set.new ['htlatex']


module BuildTools

  module Compile

    def latex_to_html(source_file)
      latex source_file, command: 'htlatex'
    end

    def latex(source_file, command: 'pdflatex')
      raise "invalid latex command #{command}" unless $valid_latex_commands.include? command
      # TODO check command is valid
      `#{command} #{source_file}`
    end

  end

  module Build

    def build(source_file, working_directory, output_directory, command)

      working_directory = Pathname working_directory

      cdir = Pathname Dir.getwd

      output_file_name = construct_output_file_name source_file

      Dir.chdir working_directory

      latex source_file, command: command

      Dir.chdir cdir

      FileUtils.cp working_directory + output_file_name, output_directory
    end

    def batch_build(source_files, working_directory, output_directory, command)

      working_directory = Pathname working_directory

      cdir = Pathname Dir.getwd

      Dir.chdir working_directory

      compiled_files = source_files.map do |file|
        latex file, command: command

        construct_output_file_name file
      end

      Dir.chdir cdir

      compiled_files.each do |file|
        FileUtils.cp working_directory + file, output_directory
      end

      def from_config(lessons, config, working_directory: nil, output_directory: nil)

        working_directory = config['working_directory'] if working_directory.nil?
        output_directory= config['outut_directory'] if output_directory.nil?
        pdf_command = config.fetch 'pdf_latex_command', 'pdflatex'
        html_command = config.fetch 'html_latex_command', 'htlatex'

        html_built = lessons.select do |file|
          config[file].fetch 'html', false
        end

        pdf_built = lessons.select do |file|
          config[file].fetch 'pdf', false
        end

        html_built = html_built.map do |file|
          config[file].fetch 'source', "lesson_#{file}.tex"
        end

        pdf_built = pdf_built.map do |file|
          config[file].fetch 'source', "lesson_#{file}.tex"
        end

        batch_build html_built, working_directory, output_directory, html_command

        batch_build pdf_built, working_directory, output_directory, pdf_command

      end

    end

    private

    def construct_output_file_name(source_file)

      base = File.basename source_file, '.*'

      base + '.html'
    end

  end

end