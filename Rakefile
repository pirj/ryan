task :default => [:compile, :test]

def output_file(input_file)
  'ebin/' + File.basename(input_file).sub(/\.\w+$/, '.beam')
end

mkdir_p 'ebin'

# Reia
ERL_SRC = FileList.new('src/**/*.erl')
ERL_SRC.each do |input|
  file output_file(input) => input do
    puts "compiling #{input}"
    sh "erlc +debug_info -o ebin #{input}"
  end
end

REIA_SRC = FileList.new('src/**/*.re')
REIA_SRC.each do |input|
  output = output_file(input)
  file output => input do
    puts "compiling #{input}"
    sh "reiac -o ebin/#{File.basename(output, ".re")} #{input}"
  end
end

PARSER_SRC = FileList.new('src/**/*.{xrl,yrl}')

task :compile => (ERL_SRC + REIA_SRC + PARSER_SRC).map { |input_file| output_file(input_file) }

# # Leex (lexer generator for Erlang)
# task :leex => ["ebin/leex.beam", "ebin/reia_scan.beam"]
# 
# file "ebin/leex.beam" => "src/leex/leex.erl" do
#   sh "#{erlc} -W0 -o ebin src/leex/leex.erl"
# end
# 
# # Compile reia_scan using leex
# file "ebin/reia_scan.beam" => %w[ebin/leex.beam src/compiler/reia_scan.xrl] do
#   sh "bin/leex src/compiler/reia_scan.xrl"
#   mv "src/compiler/reia_scan.erl", "artifacts/erl/reia_scan.erl"
#   sh "erlc +debug_info +nowarn_unused_vars -o artifacts/beam artifacts/erl/reia_scan.erl"
# end

# task :yecc => "ebin/reia_parse.beam"
# 
# # Compile reia_parse using yecc
# file "ebin/reia_parse.beam" => "src/compiler/reia_parse.yrl" do
#   sh "bin/yecc src/compiler/reia_parse.yrl"
#   mv "src/compiler/reia_parse.erl", "artifacts/erl/reia_parse.erl"
#   sh "erlc +debug_info -o ebin artifacts/erl/reia_parse.erl"
# end

task :test do
end

task :clean do
  rm_rf 'ebin'
end
