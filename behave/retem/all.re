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

  context('filtering') do
    context('capital') do
      template = renderer.parse('{a|capital}')
      should('be resolved (first letter only)') do
        assert(template.render({~a: 'apPles'}), 'ApPles')

      template = renderer.parse('{a|capitalwords}')
      should('words') do
        assert(template.render({~a: 'apPles AnD BaNaNas'}), 'Apples And Bananas')

      template = renderer.parse('{a|capitalphrase}')
      should('whole phrase') do
        assert(template.render({~a: 'apPles AnD BaNaNas'}), 'Apples and bananas')

    context('lower case') do
      template = renderer.parse('{a|lower}')
      should('be resolved') do
        assert(template.render({~a: 'apPles'}), 'apples')

    context('upper case') do
      template = renderer.parse('{a|upper}')
      should('be resolved') do
        assert(template.render({~a: 'apPles'}), 'APPLES')

    context('pretty filter') do
      template = renderer.parse('{a|pretty}')
      should('be resolved') do
        assert(template.render({~a: 'app_les'}), 'App les')

    context('inflection') do
      context('singularize') do
        template = renderer.parse('{a|singular}')
        should('be resolved') do
          assert(template.render({~a: 'apple'}), 'apple')
        should('be resolved') do
          assert(template.render({~a: 'apples'}), 'apple')

      context('pluralize') do
        template = renderer.parse('{a|plural}')
        should('be resolved') do
          assert(template.render({~a: 'apple'}), 'apples')
        should('be resolved') do
          assert(template.render({~a: 'apples'}), 'apples')

    context('list/dict length') do
      context('without units')
        template = renderer.parse('{a|length}')
        should('be resolved') do
          assert(template.render({~a: [1,2,1,5,4,3]}), '6')
        should('be resolved') do
          assert(template.render({~a: {~q: 1, ~w: 2, ~e: 3}}), '3')

      context('with units')
        template = renderer.parse("{a|length:item}")
        should('be resolved') do
          assert(template.render({~a: [1,2,1,5,4,3], ~item: {~enUS: 'item'}}), '6 items')
        should('be resolved') do
          assert(template.render({~a: [1], ~item: {~enUS: 'item'}}), '1 item')

    context('default value') do
      template = renderer.parse("{a|default:'none'}")
      should('be resolved') do
        assert(template.render({~a: 'aa'}), 'aa')
      should('be resolved') do
        assert(template.render({~a: nil}), 'none')

    context('simple textile') do
      template = renderer.parse('{a|textile}')
      should('render as emphasied')
        assert(template.render({~a: '_apple_'}), '<p><em>apple</em></p>')
      should('render as italics')
        assert(template.render({~a: '__apple__'}), '<p><i>apple</i></p>')
      should('render as strong')
        assert(template.render({~a: '*apple*'}), '<p><strong>apple</strong></p>')
      should('render as bold')
        assert(template.render({~a: '**apple**'}), '<p><b>apple</b></p>')
      should('render as deleted')
        assert(template.render({~a: '-apple-'}), '<p><del>apple</del></p>')
      should('render as inserted')
        assert(template.render({~a: '+apple+'}), '<p><ins>apple</ins></p>')
      should('render as code')
        assert(template.render({~a: '@puts("hello")@'}), '<code>puts("hello")</code>')
      should('render in span')
        assert(template.render({~a: '%apple%'}), '<p><span>apple</span></p>')
      should('render superscripted')
        assert(template.render({~a: '^apple^'}), '<p><sup>apple</sup></p>')
      should('render subscripted')
        assert(template.render({~a: '~apple~'}), '<p><sub>apple</sub></p>')
      should('render citated')
        assert(template.render({~a: '??apple??'}), '<p><cit>apple</cit></p>')

# todo: extend textile filter behave
# Block modifiers:
# hn. heading
# bq. Blockquote
# fnn. Footnote
# p. Paragraph
# bc. Block code
# pre. Pre-formatted
# # Numeric list
# * Bulleted list
# Links:
# "linktext":http://…
# Punctuation:
# "quotes" → “quotes”
# 'quotes' → ‘quotes’
# it's → it’s
# em -- dash → em — dash
# en - dash → en – dash
# 2 x 4 → 2 × 4
# foo(tm) → foo™
# foo(r) → foo®
# foo(c) → foo©
# Attributes:
# (class)
# (#id)
# {style}
# [language]
# Alignment:
# > right
# < left
# = center
# <> justify
# Tables:
# |_. a|_. table|_. header|
# |a|table|row|
# |a|table|row|
# Images:
# !imageurl!
# !imageurl!:http://…
# Acronyms:
# ABC(Always Be Closing)
# Footnotes:
# See foo[1].
# 
# fn1. Foo.
# Raw HTML:
# ==no <b>textile</b>==
# 
# notextile. no <b>textile
# here</b>
# Extended blocks:
# bq.. quote
#  
# continued quote
#  
# p. paragraph

    context('formatting a float number') do
      template = renderer.parse('{a|float:,3}')
      should('be resolved') do
        template.render({~a: 123.4567}) == '123.456'
      should('be resolved') do
        template.render({~a: 0.4567}) == '0.456'
      should('be resolved') do
        template.render({~a: 0.4}) == '0.4'

      template = renderer.parse('{a|float:4,3}')
      should('be resolved') do
        template.render({~a: 123.4567}) == '0123.456'

    context('formatting a date') do
      template = renderer.parse('{a|date:dd-MM-yy/hh:mm:ss}')
      should('be resolved') do
        template.render({~a: ((2008,12,2),(21,26,48))}) == '02-12-2008/21:26:48'

    context('formatting a relative date/time') do
      should('be resolved') do
        template = renderer.parse('{a|since:now,1}')
        template.render({~a: ((2008,12,2),(21,26,48)), ~now: ((2008,12,2),(21,30,48))}) == '5 minutes ago'
      should('be resolved') do
        template = renderer.parse('{a|until:now,2}')
        template.render({~a: ((2008,12,2),(21,26,48)), ~now: ((2008,12,2),(21,22,13))}) == 'in 4 minutes 35 seconds'

    context('formatting a size in bytes') do
      should('be resolved') do
        template = renderer.parse('{a|since:bytes,2}')
        template.render({~a: 1269.534}) == '1,269.53KB'
      should('be resolved') do
        template = renderer.parse('{a|since:bytes,2}')
        template.render({~a: 1269.554}) == '1,269.56KB'

    context('formatting a decimal in free form') do
      template = renderer.parse('{a|format:#-###-#######}')
      should('be resolved') do
        template.render({~a: 88005554567}) == '8-800-555-4567'

    context('formatting a string in free form') do
      template = renderer.parse('{a|format:#-###-#######}')
      should('be resolved') do
        template.render({~a: '8800call2me'}) == '8-800-call2me'

    context('html escaping') do
      template = renderer.parse('{a|escape}')
      should() do
        template.render({~a: '<h1>"Theory & practice"</h1>'}) == '&lt;h1&gt;&quot;Theory &amp; practice&quot;&lt;/h1&gt;'

    context('html safe') do
      template = renderer.parse('{a|safe}')
      should() do
        template.render({~a: '<h1>Theory</h1>'}) == 'Theory'

    context('text limitation') do
      should() do
        template = renderer.parse('{a|cut:15}')
        template.render({~a: 'a long long long string'}) == 'a long long ...'
      should() do
        template = renderer.parse('{a|cut:30}')
        template.render({~a: 'a long long long string'}) == 'a long long long string'
      should() do
        template = renderer.parse("{a|cut:10,''}")
        template.render({~a: 'a long long long string'}) == 'a long lon'

# postpone this until multiline strings doesn't work
# a="""a long
# long
# string"""
# {a|lines:0}
# 0 a long
# 1 long
# 2 string

    context('list join') do
      template = renderer.parse('Tags: {a|join:' '}')
      should() do
        template.render({~a: ['humour', 'life', 'friends']}) == 'Tags: humour life friends'

    context('list wrap') do
      template = renderer.parse('{a|wrap:(,)}')
      should() do
        template.render({~a: ['humour', 'life', 'friends']}) == '(humour)(life)(friends)'

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


# todo i18n:
# == i18n
# It is possible to provide custom translations dictionary in the following format:
# apps/my_app/i18n.re:
# i18n = {
#   ~apple: {
#     ~enUS: 'apple',
#     ~frFr: 'pomme'
#   },
#   ~banana: {
#     ~enUS: 'banana',
#     ~frFr: 'banane'
#   },
#   ~price: {
#     ~enUS: fun(cost, currency) {"Price: #{value} #{cur}" }
#     ~frFr: fun(cost, currency) {"Prix: #{value} #{cur}" }
#   } 
# 
# Retem uses 'en-US' as default unless otherwise requested.
# Dictionary is added to renderer instance:
# 
# renderer = Retem.new()
# 
# template = renderer.parse('{apples|count:~apples}')
# template.render({~apples: 3})
# => 3 apples
# template.render('{apples|count:~apples}', {~apples: 3}, 'fr-FR')
# => 3 pommes
# 
# todo nesting:
# It is possible to call external methods to provide nesting support.
# Imagine we have a template 'home_page', and we want it to consist of several parts:
# 
# My mail
# {nest ~mail}
# My calendar
# {nest ~calendar}
# My contacts
# {nest ~contacts}
# 
# In this case the feedback is called, and it is passed the nested atom and all the parameters.
# With this it is possible to nest templates:
# 
# renderer=Retem.new()
# template = renderer.parse('...')
# template.render({~apples: apples}) do |nested_template, params| 
#   get_template_by_name(nested_template).render(params) 
# 
# In this case, when retem sees a 'nest' statement, it calls the callback block and inserts
# the result to itself. Of course, nested parts can have nested parts too.


# todo extending filters
# todo preprocessing