class Fruits
  def index(parameters, cookies)
    bindings = {}.insert(~apples, [{~color: 'red', ~weight: 2.3, ~id: 0}, {~color: 'yellow', ~weight: 1.3, ~id: 1}])
    ('fruits_index', bindings)

  def show(parameters, cookies)
    apples = [{~color: 'red', ~weight: 2.3}, {~color: 'yellow', ~weight: 1.3}]
    # id = parameters[~id]
    # apple = apples[id]
    apple = apples[0]
    ('fruits_show', {}.insert(~apple, apple))

  def row(parameters, cookies)
    ('fruits_row', parameters)
