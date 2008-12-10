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

  def render((~identifier, atom), vars)
    vars[atom]
    
  def render((~arithmetic, ~plus, expression1, expression2), vars)
    render(expression1, vars) + render(expression2, vars)
    
  def render((~arithmetic, ~minus, expression1, expression2), vars)
    render(expression1, vars) - render(expression2, vars)
    
  def render((~arithmetic, ~divide, expression1, expression2), vars)
    render(expression1, vars) / render(expression2, vars)

  def render((~arithmetic, ~multiply, expression1, expression2), vars)
    render(expression1, vars) * render(expression2, vars)
