# template = """Total {apples|count:~apples}
# {for apple in apples} {apple.color} {apple.weight}kg {end}"""
# apples = [{~color: 'red', ~weight: 0.2}, {~color: 'yellow', ~weight: 0.15}]
# renderer = Retem.new()
# renderer.parse(template).render({~apples: apples})

module Retem
  def render(template, vars)
    tokens = retem::parse(template)
# {~arithmetic,{~identifier,aaa},{~arithmetic,{~identifier,bbb},{~identifier,ccc}}}
    retem::match(tokens, vars)
  
  # def parse({~identifier, atom})
  #   atom
  #   
  # def parse({~arithmetic, expression1, expression2})
  #   parse(expression1) 
  #   
  # def value(vars, var)
  #   vars[var]