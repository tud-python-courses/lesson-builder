require 'json'
require './builder'
require './gittools'

CONFIG_NAME = 'build_conf.json'


def build_one
  source, target = ARGV

  BuildTools::Build.build_dir_with_output source, target
end


def build_all

end


def main
  build_one
end


main if __FILE__ == $0