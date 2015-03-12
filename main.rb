load 'builder.rb'
load 'gittools.rb'
require 'json'


CONFIG_NAME = 'build_conf.json'


def main
  source, target = ARGV

  BuildTools::Build.build_directory source, target
end


main if __FILE__ == $0