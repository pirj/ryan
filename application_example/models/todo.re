class Todo < Model
  def inspect
    when_ = @data[:when]
    what = @data[:what]
    _id = @data[:_id]
    _rev = @data[:_]
    "#{when_} #{what} #{_id} #{_rev}"
  end
end

