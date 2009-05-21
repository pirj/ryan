class Todo < Model
  def initialize(what, date)
    base(:default)
    @data = {}.insert(:what, what).insert(:when, date)
  end
end

