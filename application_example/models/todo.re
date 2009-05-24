class Todo < Model
  def inspect
    when_ = @data[:when]
    what = @data[:what]
    _id = @data[:_id]
    _rev = @data[:_rev]
    "#{when_} #{what} #{_id} #{_rev}"
  end
  
  def fields # should be either static either in @metadata
    [:when, :what]
  end
end

