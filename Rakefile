task :default => [:compile]

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

REIA_SRC = FileList.new('src/example/*.re') + FileList.new('src/ryan/*.re')
REIA_SRC.each do |input|
  output = output_file(input)
  file output => input do
    puts "compiling #{input}"
    sh "reiac #{input}"
    sh 'mv *.beam ebin'
  end
end

task :compile => (ERL_SRC + REIA_SRC).map { |input_file| output_file(input_file) }

task :behave do
  sh "reia behave/all.re"
end

task :clean do
  rm_rf 'ebin'
end
