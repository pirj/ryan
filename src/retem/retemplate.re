# template = """Total {apples|count:~apples}
# {for apple in apples} {apple.color} {apple.weight}kg {end}"""
# apples = [{~color: 'red', ~weight: 0.2}, {~color: 'yellow', ~weight: 0.15}]
# renderer = Retem.new()
# renderer.parse(template).render({~apples: apples})

# T = dict:store(abc,33,dict:new()).
# T2 = dict:store(bcd,22,T).# 'Retemplate':render("{abc+bcd}",T2). => 55
# 'Retemplate':render("{abc+bcd}",T2). => 55

module Retemplate
  def render(template, vars)
    tokens = retem::parse(template)
    retem::match(vars, tokens)
  
  # def parse({~identifier, atom})
  #   atom
  #   
  # def parse({~arithmetic, expression1, expression2})
  #   parse(expression1) 
  #   
  # def value(vars, var)
  #   vars[var]