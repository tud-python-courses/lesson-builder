load 'builder.rb'
load 'gittools.rb'
require 'json'


CONFIG_NAME = 'build_conf.json'


def build_one
  source, target = ARGV

  BuildTools::Build.build_dir_with_output source, target
end


def build_all

end


def main

end


main if __FILE__ == $0