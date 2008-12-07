# Future (object/class) syntax:
# template = """Total {apples|count:~apples}
# {for apple in apples} {apple.color} {apple.weight}kg {end}"""
# apples = [{~color: 'red', ~weight: 0.2}, {~color: 'yellow', ~weight: 0.15}]
# renderer = Retem.new()
# renderer.parse(template).render({~apples: apples})

# Current syntax:
# 
# template = Retemplate.parse("{abc}")            
# Retemplate.render(template, {~abc:'AyBeeCee'}) => "AyBeeCee"
# 
# template = Retemplate.parse("{abc+bcd}")
# Retemplate.render(template, {~abc:22, ~bcd:33}) => 55
# Retemplate.render(template, {~abc:192, ~bcd:976}) => 1168

module Retemplate
  def parse(template)
    retem::parse(template)

  def render(tokens, vars)
    retem::match(vars, tokens)
  
  # def parse({~identifier, atom})
  #   atom
  #   
  # def parse({~arithmetic, expression1, expression2})
  #   parse(expression1) 
  #   
  # def value(vars, var)
  #   vars[var]