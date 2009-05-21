class Todo < Model
  def inspect
    when_ = @data[:when]
    what = @data[:what]
    "#{when_} #{what}"
  end
end

