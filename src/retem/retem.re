# Future (object/class) syntax:
# template = """Total {apples|count:~apples}
# {for apple in apples} {apple.color} {apple.weight}kg {end}"""
# apples = [{:color => 'red', :weight => 0.2}, {:color => 'yellow', :weight => 0.15}]
# renderer = Retem()
# renderer.parse(template).render({:apples => apples})

# Current syntax:
#
# template = Retem.parse("{abc+bcd} apples")
# Retem.render(template, {:abc => 22, ~bcd:33}) => 55 apples
# Retem.render(template, {:abc => 192, ~bcd:976}) => 1168 apples

module Retem
  def scan(template)
    retemplate::scan(template)
  end

  def parse(template)
    retemplate::parse(template)
  end

# empty braces
  def render((:''), vars)
    ''
  end

# reserved values
  def render((:true), vars)
    true
  end

  def render((:false), vars)
    false
  end

  def render((:nil), vars)
    nil
  end

# plain text
  def render((:text, text), vars)
    text.to_string()
  end

# get variable value from provided vars dict
  def render((:identifier, atom), vars)
    value = vars[atom]
    if value == nil
      ''
    else
      value
    end
  end

# get value
  def render((:value, value), _vars)
    value
  end

# arithmetic operators
  def render((:arithmetic, :'+', expression1, expression2), vars)
    render(expression1, vars) + render(expression2, vars)
  end

  def render((:arithmetic, :'-', expression1, expression2), vars)
    render(expression1, vars) - render(expression2, vars)
  end

  def render((:arithmetic, :'/', expression1, expression2), vars)
    render(expression1, vars) / render(expression2, vars)
  end

  def render((:arithmetic, :'*', expression1, expression2), vars)
    render(expression1, vars) * render(expression2, vars)
  end

# comparators
  def render((:comparator, :eq, expression1, expression2), vars)
    render(expression1, vars) == render(expression2, vars)
  end

  def render((:comparator, :neq, expression1, expression2), vars)
    render(expression1, vars) != render(expression2, vars)
  end

  def render((:comparator, :gt, expression1, expression2), vars)
    render(expression1, vars) > render(expression2, vars)
  end

  def render((:comparator, :lt, expression1, expression2), vars)
    render(expression1, vars) < render(expression2, vars)
  end

  def render((:comparator, :gteq, expression1, expression2), vars)
    render(expression1, vars) >= render(expression2, vars)
  end

  def render((:comparator, :lteq, expression1, expression2), vars)
    render(expression1, vars) <= render(expression2, vars)
  end

# logical operators
  def render((:logical, :and, expression1, expression2), vars)
    render(expression1, vars) and render(expression2, vars)
  end

  def render((:logical, :or, expression1, expression2), vars)
    render(expression1, vars) or render(expression2, vars)
  end

  def render((:logical, :nt, expression), vars)
    not render(expression, vars)
  end

# conditionals 
  def render((:'if', condition, statement), vars)
    if(render(condition, vars))
      render(statement, vars)
    else
      ''
    end
  end

  def render((:'unless', condition, statement), vars)
    if(!render(condition, vars))
      render(statement, vars)
    else
      ''
    end
  end
      
# nesting
  def render((:nest, controller, action, subbinding), vars)
    (:html, contents) = Ryan.page(controller, action, vars[:session], vars)
    contents.to_string()
  end

  def render((:nest, controller, action), vars)
    (:html, contents) = Ryan.page(controller, action, vars[:session], vars)
    contents.to_string()
  end

# object properties
  def render((:property, object, property), vars)
    render(object, vars)[property]
  end

# for loop
  def render((:for, var, object, block), vars)
    array = render(object, vars)
    [render(block, vars.insert(var, el)) | el in array].join()
  end

# list of blocks
  def render(list, vars)
    [render(block, vars).to_s() | block in list].join()
  end
end
