task :default => [:leex, :yecc, :compile]

def output_file(input_file)
  'ebin/' + File.basename(input_file).sub(/\.\w+$/, '.beam')
end

# Reia
ERL_SRC = FileList.new('src/**/*.erl')
ERL_SRC.each do |input|
  file output_file(input) => input do
    puts "compiling #{input}"
    sh "erlc +debug_info -o ebin #{input}"
  end
end

REIA_SRC = FileList.new('src/example/*.re') + FileList.new('src/ryan/*.re') + FileList.new('src/retem/*.re')
REIA_SRC.each do |input|
  output = output_file(input)
  file output => input do
    puts "compiling #{input}"
    sh "reiac #{input}"
    sh 'mv *.beam ebin'
  end
end

task :compile => (ERL_SRC + REIA_SRC).map { |input_file| output_file(input_file) }

# Retem Leex
task :leex => ["ebin/leex.beam", "ebin/retem_scan.beam"]

# Compile retem_scan using leex
file "ebin/retem_scan.beam" => %w[ebin/leex.beam src/retem/retem_scan.xrl] do
  sh "bin/leex src/retem/retem_scan.xrl"
  sh "erlc +debug_info -o ebin src/retem/retem_scan.erl"
  rm "src/retem/retem_scan.erl"
end

task :yecc => "ebin/retem_parse.beam"

# Compile retem_parse using yecc
file "ebin/retem_parse.beam" => "src/retem/retem_parse.yrl" do
  sh "bin/yecc src/retem/retem_parse.yrl"
  sh "erlc +debug_info -o ebin src/retem/retem_parse.erl"
  rm "src/retem/retem_parse.erl"
end

task :behave do
  sh "reia behave/all.re"
end

task :clean do
  sh 'rm ebin/*'
end

task :install do
  lib_dir = `erl -noshell -eval "io:format(code:lib_dir())" -s init stop`
  ryan_dir = File.join(lib_dir, 'ryan', '')
  
  rm_r ryan_dir if File.exist?(ryan_dir)
  mkdir ryan_dir
  
  %w[LICENSE README.markdown ebin].each { |f| cp_r f, ryan_dir }
  
  mkdir "/usr/local/bin" unless File.exist?("/usr/local/bin")
  cp 'bin/ryan', '/usr/local/bin'
  
  File.chmod 0755, "/usr/local/bin/ryan"
end