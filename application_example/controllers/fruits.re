class Fruits
  def initialize
    @apples = [{~color: 'red', ~weight: 2.3, ~id: 0}, {~color: 'yellow', ~weight: 1.3, ~id: 1}, {~color: 'green', ~weight: 0.6, ~id: 2}]

  def index(parameters, cookies)
    bindings = {}.insert(~apples, @apples)
    ('fruits/index', bindings)

  def show(parameters, cookies)
    id = parameters[~id]
    if(id == nil)
      ""
    else
      apple = @apples[id.to_int()]
      ('fruits/show', {}.insert(~apple, apple))

  def row(parameters, cookies)
    ('fruits/row', parameters)
