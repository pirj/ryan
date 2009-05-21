Main.puts('Running example application')
todos = [{:what => 'buy milk', :when => :today}, {:what => 'call parents', :when => :tomorrow}, {:what => 'visit dentist', :when => :few_days}]

todos.each do |todo|
  Todo(todo[:what], todo[:when]).save()
end

