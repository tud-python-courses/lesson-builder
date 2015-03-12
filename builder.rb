require 'pathname'
require 'set'
require 'logger'
require 'fileutils'


module BuildTools

  VALID_LATEX_COMMANDS = {:htlatex => 'html', :pdflatex => 'pdf'}

  module Compile

    # call a latex command with 'htlatex' as default
    def self.latex_to_html(source_file, command: :htlatex)
      latex source_file, command: command
    end

    # call a latex command and return useful information
    def self.latex(source_file, command: :pdflatex)

      command = command.to_sym

      return_type = VALID_LATEX_COMMANDS[command]

      raise "invalid latex command #{command}" if return_type.nil?

      c = "#{command} -halt-on-error #{source_file}"
      [system("#{c}"), construct_out_file_name(source_file, return_type)]
    end

    private

    # helper method clue is in the name
    def self.construct_out_file_name(path, type)
      path = Pathname path unless path.is_a? Pathname

      File.basename(path, '.*') + ".#{type}"
    end

  end

  module Build

    # build a single source file
    def self.build(source_file, source_dir, target_dir, command)

      source_dir = Pathname source_dir

      cdir = Pathname Dir.getwd

      Dir.chdir source_dir

      success, output_file_name = Compile.latex source_file, command: command

      Dir.chdir cdir

      FileUtils.cp(source_dir + output_file_name, target_dir) if success

      [success, output_file_name]
    end

    # build multiple source files
    def self.batch_build(source_files, source_dir, target_dir, command)

      source_dir = Pathname source_dir

      cdir = Pathname Dir.getwd

      Dir.chdir source_dir

      compile_result = source_files.map do |file|
        Compile.latex file, command: command
      end

      Dir.chdir cdir


      compile_result.zip(source_files).map do |result, source|
        success, file = result

        FileUtils.cp(source_dir + file, target_dir) if success

        [success, source_dir + source, target_dir + file]
      end

    end

    # Build all objects(hashes) in config based on the settings in the config hash
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
        lesson.fetch 'source', "lesson_#{lesson['number']}.tex"
      end

      pdf_built = pdf_built.map do |lesson|
        lesson.fetch 'source', "lesson_#{lesson['number']}.tex"
      end

      # build html
      html_res = batch_build html_built, source_dir, target_dir, html_command

      # build pdf
      pdf_res = batch_build pdf_built, source_dir, target_dir, pdf_command

      { :html => html_res, :pdf => pdf_res }

    end

    # Build all source files in a directory based on a build_conf.json in that directory
    def self.build_directory(source, target)
      source = Pathname source

      conf = JSON.parse File.read(source + CONFIG_NAME)

      BuildTools::Build.from_config conf, source_dir: source, target_dir: target
    end

    # Same as build_directory but also prints some useful output to the console
    def self.build_dir_with_output(source, target)
      start_time = Time.new

      built = build_directory source, target

      puts "Built #{built[:html].length + built[:pdf].length} files in #{Time.new - start_time}"
      built.each do |_, list|
        list.each do |result, original, compiled|
          if result
            result = 'success'
          else
            result = 'errored'
          end

          puts "#{original}  ->  #{compiled}   #{result}"
        end
      end
    end
  end

end