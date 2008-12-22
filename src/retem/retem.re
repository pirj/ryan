# Future (object/class) syntax:
# template = """Total {apples|count:~apples}
# {for apple in apples} {apple.color} {apple.weight}kg {end}"""
# apples = [{~color: 'red', ~weight: 0.2}, {~color: 'yellow', ~weight: 0.15}]
# renderer = Retem.new()
# renderer.parse(template).render({~apples: apples})

# Current syntax:
#
# template = Retem.parse("{abc+bcd} apples")
# Retem.render(template, {~abc:22, ~bcd:33}) => 55 apples
# Retem.render(template, {~abc:192, ~bcd:976}) => 1168 apples

module Retem
  def scan(template)
    retemplate::scan(template)

  def parse(template)
    retemplate::parse(template)

# empty braces
  def render((~''), vars)
    ''

# reserved values
  def render((~true), vars)
    true

  def render((~false), vars)
    false

  def render((~nil), vars)
    nil

# plain text
  def render((~text, text), vars)
    text.to_string()

# get variable value from provided vars dict
  def render((~identifier, atom), vars)
    value = vars[atom]
    if value == nil
      ''
    else
      value

# get value
  def render((~value, value), _vars)
    value

# arithmetic operators
  def render((~arithmetic, ~'+', expression1, expression2), vars)
    render(expression1, vars) + render(expression2, vars)

  def render((~arithmetic, ~'-', expression1, expression2), vars)
    render(expression1, vars) - render(expression2, vars)

  def render((~arithmetic, ~'/', expression1, expression2), vars)
    render(expression1, vars) / render(expression2, vars)

  def render((~arithmetic, ~'*', expression1, expression2), vars)
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

# conditionals 
  def render((~condition, ~'if', condition, statement), vars)
    if(render(condition, vars))
      render(statement, vars)
    else
      ''

  def render((~condition, ~'unless', condition, statement), vars)
    if(!render(condition, vars))
      render(statement, vars)
    else
      ''
      
# nesting
  def render((~nest, sub), vars)
    subbindings = vars[sub]
    page = subbindings[~page]
    if page == nil
      ''
    else
      Ryan.page(page.to_s(), subbindings)

# list of blocks
  def render(list, vars)
    [render(block, vars).to_s() | block in list].join()
