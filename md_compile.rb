require 'github/markdown'

input_file = ARGV[0]

puts GitHub::Markdown.render(File.read(input_file))
