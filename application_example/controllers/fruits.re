class Fruits
  def initialize
    @apples = [{~color: 'red', ~weight: 2.3, ~id: 0}, {~color: 'yellow', ~weight: 1.3, ~id: 1}]

  def index(parameters, cookies)
    bindings = {}.insert(~apples, @apples)
    ('fruits/index', bindings)

  def show(parameters, cookies)
    id = parameters[~id].to_int()
    apple = @apples[id]
    ('fruits/show', {}.insert(~apple, apple))

  def row(parameters, cookies)
    ('fruits/row', parameters)
