# Future (object/class) syntax:
# template = """Total {apples|count:~apples}
# {for apple in apples} {apple.color} {apple.weight}kg {end}"""
# apples = [{~color: 'red', ~weight: 0.2}, {~color: 'yellow', ~weight: 0.15}]
# renderer = Retem.new()
# renderer.parse(template).render({~apples: apples})

# Current syntax:
# 
# template = Retem.parse("{abc}")            
# Retem.render(template, {~abc:'AyBeeCee'}) => "AyBeeCee"
# 
# template = Retem.parse("{abc+bcd}")
# Retem.render(template, {~abc:22, ~bcd:33}) => 55
# Retem.render(template, {~abc:192, ~bcd:976}) => 1168

module Retem
  def parse(template)
    retemplate::parse(template)

# get variable value from provided vars dict
  def render((~identifier, atom), vars)
    vars[atom]
    
# arithmetic operators
  def render((~arithmetic, ~plus, expression1, expression2), vars)
    render(expression1, vars) + render(expression2, vars)
    
  def render((~arithmetic, ~minus, expression1, expression2), vars)
    render(expression1, vars) - render(expression2, vars)
    
  def render((~arithmetic, ~divide, expression1, expression2), vars)
    render(expression1, vars) / render(expression2, vars)

  def render((~arithmetic, ~multiply, expression1, expression2), vars)
    render(expression1, vars) * render(expression2, vars)

# comparators
  def render((~comparator, ~eq, expression1, expression2), vars)
    render(expression1, vars) == render(expression2, vars)

  def render((~comparator, ~neq, expression1, expression2), vars)
    render(expression1, vars) != render(expression2, vars)

  def render((~comparator, ~gt, expression1, expression2), vars)
    render(expression1, vars) > render(expression2, vars)

  def render((~comparator, ~lt, expression1, expression2), vars)
    render(expression1, vars) < render(expression2, vars)

  def render((~comparator, ~gteq, expression1, expression2), vars)
    render(expression1, vars) >= render(expression2, vars)

  def render((~comparator, ~lteq, expression1, expression2), vars)
    render(expression1, vars) <= render(expression2, vars)

# logical operators
  def render((~logical, ~and, expression1, expression2), vars)
    render(expression1, vars) and render(expression2, vars)

  def render((~logical, ~or, expression1, expression2), vars)
    render(expression1, vars) or render(expression2, vars)

  def render((~logical, ~not, expression), vars)
    not render(expression, vars)

