class Store
  def set_address(address)
    @address = address
  def set_name(name)
    @name = name

class Address
  def set_street(street)
    @street = street

Behave.context('Templating') do
  renderer = Retem.new()

  context('text') do
    string = 'la 123%5442!@#$%^&*()|":LK<MN~123efgsfd"'
    template = renderer.parse(string)
    should('be resolved with no changes') do
      template.render({~a: 100}) == string
    
  context('empty braces') do
    should('be resolved to an empty string') do
      assert(renderer.parse('{}').render(), '')
      assert(renderer.parse('{ }').render(), '')

  context('variable') do
    should('be resolved (numeric)') do
      Retem.render('{a}', {~a: 100}) == '100'
    should('be resolved (alpha)') do
      Retem.render('{a}', {~a: 'abc'}) == 'abc'

  context('variable referencing a variable') do
    should('be resolved') do
      Retem.render('{a}', {~a: ~b, ~b: 'abc'}) == 'abc'

  context('variable properties') do
    address = Address.start()
    address.set_street('main st.')
    store = Store.start()
    store.set_address(address)
    store.set_name('store name')
    should('be resolved') do
      Retem.render('{s.name}', {~s: store}) == 'store name'
    should('be deep resolved') do
      Retem.render('{s.address.street}', {~s: store}) == 'main st.'

  context('flow control') do
    context('if statement') do
      template = renderer.parse('{if a}yes{end}')
      should('be resolved') do
        assert(template.render({~a: true}), 'yes')
        assert(template.render({~a: false}), '')
    
    context('if/else statement') do
      template = renderer.parse('{if a}yes{else}no{end}')
      should('be resolved') do
        assert(template.render({~a: true}), 'yes')
        assert(template.render({~a: false}), 'no')

    context('if/else/elseif statement') do
      template = renderer.parse('{if a}yes, a{elseif b}yes, b{else}no{end}')
      should('be resolved') do
        assert(template.render({~a: true, ~b: true}), 'yes, a')
        assert(template.render({~a: false, ~b: true}), 'yes, b')
        assert(template.render({~a: false, ~b: false}), 'no')

    context('unless statement') do
      template = renderer.parse('{unless a}no{end}')
      should('be resolved') do
        assert(template.render({~a: true}), '')
        assert(template.render({~a: false}), 'no')

    context('logical operators') do
      context('(and)') do
        template = renderer.parse('{if a and b}both{end}')
        should('be resolved') do
          assert(template.render({~a: true, ~b: true}), 'both')
          assert(template.render({~a: true, ~b: false}), '')
          assert(template.render({~a: false, ~b: true}), '')
          assert(template.render({~a: false, ~b: false}), '')

      context('(or)') do
        template = renderer.parse('{if a or b}yes{end}')
        should('be resolved') do
          assert(template.render({~a: true, ~b: true}), 'yes')
          assert(template.render({~a: true, ~b: false}), 'yes')
          assert(template.render({~a: false, ~b: true}), 'yes')
          assert(template.render({~a: false, ~b: false}), '')

    context('comparsion operators') do
      context('(eq)') do
        template = renderer.parse('{if a eq b}equal{end}')
        should('be resolved') do
          assert(template.render({~a: 123, ~b: 321}), '')
          assert(template.render({~a: 123, ~b: 123}), 'equal')
          assert(template.render({~a: true, ~b: false}), '')
          assert(template.render({~a: true, ~b: true}), 'equal')
          assert(template.render({~a: 'abc', ~b: 'cba'}), '')
          assert(template.render({~a: 'abc', ~b: 'abc'}), 'equal')

      context('(neq)') do
        template = renderer.parse('{if a neq b}not equal{end}')
        should('be resolved') do
          assert(template.render({~a: 123, ~b: 321}), 'not equal')
          assert(template.render({~a: 123, ~b: 123}), '')
          assert(template.render({~a: true, ~b: false}), 'not equal')
          assert(template.render({~a: true, ~b: true}), '')
          assert(template.render({~a: 'abc', ~b: 'cba'}), 'not equal')
          assert(template.render({~a: 'abc', ~b: 'abc'}), '')

      context('(gt)') do
        template = renderer.parse('{if a gt b}greather{end}')
        should('be resolved') do
          assert(template.render({~a: 123, ~b: 321}), '')
          assert(template.render({~a: 123, ~b: 123}), '')
          assert(template.render({~a: 321, ~b: 123}), 'greather')
          assert(template.render({~a: true, ~b: false}), 'greather')
          assert(template.render({~a: true, ~b: true}), '')
          assert(template.render({~a: false, ~b: true}), '')
          assert(template.render({~a: 'abc', ~b: 'cba'}), '')
          assert(template.render({~a: 'abc', ~b: 'abc'}), '')
          assert(template.render({~a: 'cba', ~b: 'abc'}), 'greather')

      context('(lt)') do
        template = renderer.parse('{if a lt b}less{end}')
        should('be resolved') do
          assert(template.render({~a: 123, ~b: 321}), 'less')
          assert(template.render({~a: 123, ~b: 123}), '')
          assert(template.render({~a: 321, ~b: 123}), '')
          assert(template.render({~a: true, ~b: false}), '')
          assert(template.render({~a: true, ~b: true}), '')
          assert(template.render({~a: false, ~b: true}), 'less')
          assert(template.render({~a: 'abc', ~b: 'cba'}), 'less')
          assert(template.render({~a: 'abc', ~b: 'abc'}), '')
          assert(template.render({~a: 'cba', ~b: 'abc'}), '')

      context('(gteq)') do
        template = renderer.parse('{if a gteq b}greather or eq{end}')
        should('be resolved') do
          assert(template.render({~a: 123, ~b: 321}), '')
          assert(template.render({~a: 123, ~b: 123}), 'greather or eq')
          assert(template.render({~a: 321, ~b: 123}), 'greather or eq')
          assert(template.render({~a: true, ~b: false}), 'greather or eq')
          assert(template.render({~a: true, ~b: true}), 'greather or eq')
          assert(template.render({~a: false, ~b: true}), '')
          assert(template.render({~a: 'abc', ~b: 'cba'}), '')
          assert(template.render({~a: 'abc', ~b: 'abc'}), 'greather or eq')
          assert(template.render({~a: 'cba', ~b: 'abc'}), 'greather or eq')

      context('(lteq)') do
        template = renderer.parse('{if a lteq b}less or eq{end}')
        should('be resolved') do
          assert(template.render({~a: 123, ~b: 321}), 'less or eq')
          assert(template.render({~a: 123, ~b: 123}), 'less or eq')
          assert(template.render({~a: 321, ~b: 123}), '')
          assert(template.render({~a: true, ~b: false}), '')
          assert(template.render({~a: true, ~b: true}), 'less or eq')
          assert(template.render({~a: false, ~b: true}), 'less or eq')
          assert(template.render({~a: 'abc', ~b: 'cba'}), 'less or eq')
          assert(template.render({~a: 'abc', ~b: 'abc'}), 'less or eq')
          assert(template.render({~a: 'cba', ~b: 'abc'}), '')

    context('mixed non-variable/variable comparsions') do
      context('numeric') do
        template = renderer.parse('{if a gt 132}greather{end}')
        should('be resolved') do
          assert(template.render({~a: 123}), '')
          assert(template.render({~a: 321}), 'greather')

      context('boolean') do
        template = renderer.parse('{if a gt false}greather{end}')
        should('be resolved') do
          assert(template.render({~a: true}), 'greather')
          assert(template.render({~a: false}), '')

      context('string') do
        template = renderer.parse('{if a gt "abc"}greather{end}')
        should('be resolved') do
          assert(template.render({~a: 'abc'}), '')
          assert(template.render({~a: 'cba'}), 'greather')

    context('composite comparsions') do
      context('chained') do
        template = renderer.parse('{if a or b and c}a or b and c{end}')
        should('be resolved') do
          assert(template.render({~a: false, ~b: false, ~c: false}), '')
          assert(template.render({~a: true, ~b: false, ~c: false}), '')
          assert(template.render({~a: false, ~b: true, ~c: false}), '')
          assert(template.render({~a: true, ~b: true, ~c: false}), '')
          assert(template.render({~a: false, ~b: false, ~c: true}), '')
          assert(template.render({~a: true, ~b: false, ~c: true}), 'a or b and c')
          assert(template.render({~a: false, ~b: true, ~c: true}), 'a or b and c')
          assert(template.render({~a: true, ~b: false, ~c: true}), 'a or b and c')

      context('compound') do
        template = renderer.parse('{if a gt 3 and b lt 3}a > 3 and b < 3{end}')
        should('be resolved') do
          assert(template.render({~a: 1, ~b: 1}), '')
          assert(template.render({~a: 3, ~b: 1}), '')
          assert(template.render({~a: 5, ~b: 1}), 'a > 3 and b < 3')
          assert(template.render({~a: 1, ~b: 3}), '')
          assert(template.render({~a: 3, ~b: 3}), '')
          assert(template.render({~a: 5, ~b: 3}), '')
          assert(template.render({~a: 1, ~b: 5}), '')
          assert(template.render({~a: 3, ~b: 5}), '')
          assert(template.render({~a: 5, ~b: 5}), '')

  context('iterating through') do
    context('a list') do
      template = renderer.parse('{for x in many}{x}{end}')
      should('be resolved') do
        assert(template.render({~many: [1, 2, 3]}), '123')
        assert(template.render({~many: ['a', 'b', 'c']}), 'abc')

    context('a tuple') do
      template = renderer.parse('{for x in many}{x}{end}')
      should('be resolved') do
        assert(template.render({~many: (1, 2, 3)}), '123')
        assert(template.render({~many: ('a', 'b', 'c')ght}), 'abc')

    context('a dict') do
      template = renderer.parse('{for {k:v} in many}-{k}:{v}{end}')
      should('be resolved') do
        assert(template.render({~many:{~a: 3, ~b: 4}}), '-a:3-b:4')
        assert(template.render({~many:{'a': 3, 'b': 4}}), '-a:3-b:4')
        assert(template.render({~many:{'a': 'q', 'b': 'w'}}), '-a:q-b:w')

  context('iterating with pattern matching through') do
    context('a list') do
      template = renderer.parse('{for [a,b] in many}-{a}:{b}{end}')
      should('be resolved') do
        assert(template.render({~many: [['aaa', 123], ['qqq', 456]]}), '-aaa:123-qqq:456')

    context('a list of tuples') do
      template = renderer.parse('{for (a,b,c) in many}-{a}:{b}:{c}{end}')
      should('be resolved') do
        assert(template.render({~many: [('aaa', 123, 'www'), ('qqq', 456, 'zzz')]}), '-aaa:123:www-qqq:456:zzz')

    context('a list of dicts/objects') do
      template = renderer.parse('{for {color: ~color, weight: ~weight} in apples}-{color}:{weight}{end}')
      should('be resolved') do
        assert(template.render({~apples = [{~color: 'red', ~weight: 0.2}, {~color: 'yellow', ~weight: 0.15}]}), '-red:0.2-yellow:0.15')

# todo filtering
# a='apPles'
# {a|capital}
# => ApPles
# 
# a='apPles AnD BaNaNas'
# {a|capitalphrase}
# => Apples and bananas
# 
# a='apPles'
# {a|lower}
# => apples
# 
# a='apPles'
# {a|upper}
# => APPLES
# 
# a='app_les'
# {a|pretty}
# => App les
# 
# a='apple'
# {a|plural}
# => apples
# 
# a='apples'
# {a|singular}
# => apple
# 
# a='apples'
# {a|length}
# => 6
# 
# a=nil
# {a|default:'none'}
# => none
# 
# a='*apple*'
# {a|textile}
# => <b>apple</b>
# 
# a='/apple/'
# {a|textile}
# => <i>apple</i>
# 
# a=123
# {a|float:0,3}
# => 123.000
# 
# a=123
# {a|float:4,3}
# => 0123.000
# 
# a=Time.now
# {a|date:dd-MM-yy/hh:mm:ss}
# => 11-11-2008/02:31:14
# 
# a=Time.now - 100
# {a|since:1}
# => 2 minutes
# 
# a=Time.now + 100
# {a|until:2}
# => 1 minute, 40 seconds
# 
# a=1300000
# {a|bytes:2}
# => 1,269.53KB
# 
# a=88005554567
# {a|format:#-###-###-####}
# => 8-800-555-4567
# 
# a='8800call2me'
# {a|format:#-###-#######}
# => 8-800-call2me
# 
# a='<h1>"Theory & practice"</h1>'
# {a|escape}
# => &lt;h1&gt;&quot;Theory &amp; practice&quot;&lt;/h1&gt;
# 
# a='<h1>Theory</h1>'
# {a|safe}
# => Theory
# 
# a='a long long long string'
# {a|cut:15}
# => a long long ...
# {a|cut:30}
# => a long long long string
# 
# a="""a long
# long
# string"""
# {a|lines:0}
# 0 a long
# 1 long
# 2 string
# 
# a=['apple', 'banana', 'coconut']
# {a|count}
# => 3
# 
# a=['humour', 'life', 'friends']
# Tags: {a|join:' '}
# => Tags: humour life friends
# 
# Filters can take several parameters:
# a=['humour', 'life', 'friends']
# {a|wrap:(,)}
# => (humour)(life)(friends)
# 
# {a|cut:10,''}
# => a long lon
# 
# a=[123, 345, 567]
# {a|count:'item','items'}
# => 3 items
# 
# a=[123, 345, 567]
# - kilo !countable
#   en-US: kilogram, kilograms
#   fr-FR: kilogramme, kilogrammes
# {a|count:~kilo}
# => 3 kilograms
# 
# Filters can be chained:
# a='appLes'
# {a|lower|capital|singular|cut:10}
# => Apple
# 
# a=['humour', 'life', 'friends']
# {a|wrap:(,)|join:'/'}
# => (humour)/(life)/(friends)
# 
# a='hello, world'
# {a|wrap:*|textile}
# => <b>hello, world</b>
# 
# a='hello, world'
# {a|split:' ',','|capital|join:/}
# => Hello/World
# 
# Filters can take variables as parameters:
# a='long long long string'
# max_length=10
# {a|cut:max_length}
# => long lo...
# 
# Filters can walk through data:
# a=[('apples', 11),('bananas', 32)]
# {a|sum:(_,x)}
# => 43
# 
# a=[{~apples: 11},{~bananas: 32}]
# {a|sum:{_,x}}
# => 43
# 
# a=[[111, 222], [78, 20]]
# {a|sum:[_,x]}
# => 242


# todo i18n

# todo nesting

# todo extending filters