class Store
  def set_address(address)
    @address = address
  end
  def set_name(name)
    @name = name
  end
end

class Address
  def set_street(street)
    @street = street
  end
end

Behave.context('Templating') do
  renderer = Retem()

  context('text') do
    string = 'la 123%5442!@#$%^&*()|":LK<MN~123efgsfd"'
    template = renderer.parse(string)
    should('be resolved with no changes') do
      template.render({:a => 100}) == string
    end
  end

  context('empty braces') do
    should('be resolved to an empty string') do
      assert(renderer.parse('{}').render() == '')
      assert(renderer.parse('{ }').render() == '')
    end
  end

  context('variable') do
    should('be resolved (numeric)') do
      Retem.render('{a}', {:a => 100}) == '100'
    end
    should('be resolved (alpha)') do
      Retem.render('{a}', {:a => 'abc'}) == 'abc'
    end
  end

  context('variable referencing a variable') do
    should() do
      Retem.render('{a}', {:a => :b, :b => 'abc'}) == 'abc'
    end

  context('variable properties') do
    address = Address()
    address.set_street('main st.')
    store = Store()
    store.set_address(address)
    store.set_name('store name')
    should() do
      Retem.render('{s.name}', {:s => store}) == 'store name'
    end
    should('be deep resolved') do
      Retem.render('{s.address.street}', {:s => store}) == 'main st.'
    end
  end

  context('flow control') do
    context('if statement') do
      template = renderer.parse('{if a}yes{end}')
      should() do
        assert(template.render({:a => true}) == 'yes')
        assert(template.render({:a => false}) == '')
      end
    end

    context('if/else statement') do
      template = renderer.parse('{if a}yes{else}no{end}')
      should() do
        assert(template.render({:a => true}) == 'yes')
        assert(template.render({:a => false}) == 'no')
      end
    end

    context('if/else/elseif statement') do
      template = renderer.parse('{if a}yes, a{elseif b}yes, b{else}no{end}')
      should() do
        assert(template.render({:a => true, :b => true}) == 'yes, a')
        assert(template.render({:a => false, :b => true}) == 'yes, b')
        assert(template.render({:a => false, :b => false}) == 'no')
      end
    end

    context('unless statement') do
      template = renderer.parse('{unless a}no{end}')
      should() do
        assert(template.render({:a => true}) == '')
        assert(template.render({:a => false}) == 'no')
      end
    end

    context('logical operators') do
      context('(and)') do
        template = renderer.parse('{if a and b}both{end}')
        should() do
          assert(template.render({:a => true, :b => true}) == 'both')
          assert(template.render({:a => true, :b => false}) == '')
          assert(template.render({:a => false, :b => true}) == '')
          assert(template.render({:a => false, :b => false}) == '')
        end
      end

      context('(or)') do
        template = renderer.parse('{if a or b}yes{end}')
        should() do
          assert(template.render({:a => true, :b => true}) == 'yes')
          assert(template.render({:a => true, :b => false}) == 'yes')
          assert(template.render({:a => false, :b => true}) == 'yes')
          assert(template.render({:a => false, :b => false}) == '')
        end
      end
    end

    context('comparsion operators') do
      context('(eq)') do
        template = renderer.parse('{if a eq b}equal{end}')
        should() do
          assert(template.render({:a => 123, :b => 321}) == '')
          assert(template.render({:a => 123, :b => 123}) == 'equal')
          assert(template.render({:a => true, :b => false}) == '')
          assert(template.render({:a => true, :b => true}) == 'equal')
          assert(template.render({:a => 'abc', :b => 'cba'}) == '')
          assert(template.render({:a => 'abc', :b => 'abc'}) == 'equal')
        end
      end

      context('(neq)') do
        template = renderer.parse('{if a neq b}not equal{end}')
        should() do
          assert(template.render({:a => 123, :b => 321}) == 'not equal')
          assert(template.render({:a => 123, :b => 123}) == '')
          assert(template.render({:a => true, :b => false}) == 'not equal')
          assert(template.render({:a => true, :b => true}) == '')
          assert(template.render({:a => 'abc', :b => 'cba'}) == 'not equal')
          assert(template.render({:a => 'abc', :b => 'abc'}) == '')
        end
      end

      context('(gt)') do
        template = renderer.parse('{if a gt b}greather{end}')
        should() do
          assert(template.render({:a => 123, :b => 321}) == '')
          assert(template.render({:a => 123, :b => 123}) == '')
          assert(template.render({:a => 321, :b => 123}) == 'greather')
          assert(template.render({:a => true, :b => false}) == 'greather')
          assert(template.render({:a => true, :b => true}) == '')
          assert(template.render({:a => false, :b => true}) == '')
          assert(template.render({:a => 'abc', :b => 'cba'}) == '')
          assert(template.render({:a => 'abc', :b => 'abc'}) == '')
          assert(template.render({:a => 'cba', :b => 'abc'}) == 'greather')
        end
      end

      context('(lt)') do
        template = renderer.parse('{if a lt b}less{end}')
        should() do
          assert(template.render({:a => 123, :b => 321}) == 'less')
          assert(template.render({:a => 123, :b => 123}) == '')
          assert(template.render({:a => 321, :b => 123}) == '')
          assert(template.render({:a => true, :b => false}) == '')
          assert(template.render({:a => true, :b => true}) == '')
          assert(template.render({:a => false, :b => true}) == 'less')
          assert(template.render({:a => 'abc', :b => 'cba'}) == 'less')
          assert(template.render({:a => 'abc', :b => 'abc'}) == '')
          assert(template.render({:a => 'cba', :b => 'abc'}) == '')
        end
      end

      context('(gteq)') do
        template = renderer.parse('{if a gteq b}greather or eq{end}')
        should() do
          assert(template.render({:a => 123, :b => 321}) == '')
          assert(template.render({:a => 123, :b => 123}) == 'greather or eq')
          assert(template.render({:a => 321, :b => 123}) == 'greather or eq')
          assert(template.render({:a => true, :b => false}) == 'greather or eq')
          assert(template.render({:a => true, :b => true}) == 'greather or eq')
          assert(template.render({:a => false, :b => true}) == '')
          assert(template.render({:a => 'abc', :b => 'cba'}) == '')
          assert(template.render({:a => 'abc', :b => 'abc'}) == 'greather or eq')
          assert(template.render({:a => 'cba', :b => 'abc'}) == 'greather or eq')
        end
      end

      context('(lteq)') do
        template = renderer.parse('{if a lteq b}less or eq{end}')
        should() do
          assert(template.render({:a => 123, :b => 321}) == 'less or eq')
          assert(template.render({:a => 123, :b => 123}) == 'less or eq')
          assert(template.render({:a => 321, :b => 123}) == '')
          assert(template.render({:a => true, :b => false}) == '')
          assert(template.render({:a => true, :b => true}) == 'less or eq')
          assert(template.render({:a => false, :b => true}) == 'less or eq')
          assert(template.render({:a => 'abc', :b => 'cba'}) == 'less or eq')
          assert(template.render({:a => 'abc', :b => 'abc'}) == 'less or eq')
          assert(template.render({:a => 'cba', :b => 'abc'}) == '')
        end
      end
    end

    context('mixed non-variable/variable comparsions') do
      context('numeric') do
        template = renderer.parse('{if a gt 132}greather{end}')
        should() do
          assert(template.render({:a => 123}) == '')
          assert(template.render({:a => 321}) == 'greather')
        end
      end

      context('boolean') do
        template = renderer.parse('{if a gt false}greather{end}')
        should() do
          assert(template.render({:a => true}) == 'greather')
          assert(template.render({:a => false}) == '')
        end
      end

      context('string') do
        template = renderer.parse('{if a gt "abc"}greather{end}')
        should() do
          assert(template.render({:a => 'abc'}) == '')
          assert(template.render({:a => 'cba'}) == 'greather')
        end
      end
    end

    context('composite comparsions') do
      context('chained') do
        template = renderer.parse('{if a or b and c}a or b and c{end}')
        should() do
          assert(template.render({:a => false, :b => false, :c => false}) == '')
          assert(template.render({:a => true, :b => false, :c => false}) == '')
          assert(template.render({:a => false, :b => true, :c => false}) == '')
          assert(template.render({:a => true, :b => true, :c => false}) == '')
          assert(template.render({:a => false, :b => false, :c => true}) == '')
          assert(template.render({:a => true, :b => false, :c => true}) == 'a or b and c')
          assert(template.render({:a => false, :b => true, :c => true}) == 'a or b and c')
          assert(template.render({:a => true, :b => false, :c => true}) == 'a or b and c')
        end
      end

      context('compound') do
        template = renderer.parse('{if a gt 3 and b lt 3}a > 3 and b < 3{end}')
        should() do
          assert(template.render({:a => 1, :b => 1}) == '')
          assert(template.render({:a => 3, :b => 1}) == '')
          assert(template.render({:a => 5, :b => 1}) == 'a > 3 and b < 3')
          assert(template.render({:a => 1, :b => 3}) == '')
          assert(template.render({:a => 3, :b => 3}) == '')
          assert(template.render({:a => 5, :b => 3}) == '')
          assert(template.render({:a => 1, :b => 5}) == '')
          assert(template.render({:a => 3, :b => 5}) == '')
          assert(template.render({:a => 5, :b => 5}) == '')
        end
      end
    end
  end

  context('iterating through') do
    context('a list') do
      template = renderer.parse('{for x in many}{x}{end}')
      should() do
        assert(template.render({:many => [1, 2, 3]}) == '123')
        assert(template.render({:many => ['a', 'b', 'c']}) == 'abc')
      end
    end

    context('a tuple') do
      template = renderer.parse('{for x in many}{x}{end}')
      should() do
        assert(template.render({:many => (1, 2, 3)}) == '123')
        assert(template.render({:many => ('a', 'b', 'c')ght}) == 'abc')
      end
    end

    context('a dict') do
      template = renderer.parse('{for {k:v} in many}-{k}:{v}{end}')
      should() do
        assert(template.render({:many:{:a => 3, :b => 4}}) == '-a:3-b:4')
        assert(template.render({:many:{'a' => 3, 'b' => 4}}) == '-a:3-b:4')
        assert(template.render({:many:{'a' => 'q', 'b' => 'w'}}) == '-a:q-b:w')
      end
    end
  end

  context('iterating with pattern matching through') do
    context('a list') do
      template = renderer.parse('{for [a,b] in many}-{a}:{b}{end}')
      should() do
        template.render({:many => [['aaa', 123], ['qqq', 456]]}) == '-aaa:123-qqq:456'
      end
    end

    context('a list of tuples') do
      template = renderer.parse('{for (a,b,c) in many}-{a}:{b}:{c}{end}')
      should() do
        template.render({:many => [('aaa', 123, 'www'), ('qqq', 456, 'zzz')]}) == '-aaa:123:www-qqq:456:zzz'
      end
    end

    context('a list of dicts/objects') do
      template = renderer.parse('{for {color: :color, weight: :weight} in apples}-{color}:{weight}{end}')
      should() do
        template.render({:apples = [{:color => 'red', :weight => 0.2}, {:color => 'yellow', :weight => 0.15}]}) == '-red:0.2-yellow:0.15'
      end
    end
  end

  context('filtering') do
    context('capital') do
      template = renderer.parse('{a|capital}')
      should('be resolved (first letter only)') do
        template.render({:a => 'apPles'}) == 'ApPles'
      end

      template = renderer.parse('{a|capitalwords}')
      should('words') do
        template.render({:a => 'apPles AnD BaNaNas'}) == 'Apples And Bananas'
      end

      template = renderer.parse('{a|capitalphrase}')
      should('whole phrase') do
        template.render({:a => 'apPles AnD BaNaNas'}) == 'Apples and bananas'
      end
    end

    context('lower case') do
      template = renderer.parse('{a|lower}')
      should() do
        template.render({:a => 'apPles'}) == 'apples'
      end
    end

    context('upper case') do
      template = renderer.parse('{a|upper}')
      should() do
        template.render({:a => 'apPles'}) == 'APPLES'
      end
    end

    context('pretty filter') do
      template = renderer.parse('{a|pretty}')
      should() do
        template.render({:a => 'app_les'}) == 'App les'
      end
    end

    context('inflection') do
      context('singularize') do
        template = renderer.parse('{a|singular}')
        should() do
          assert(template.render({:a => 'apple'}), 'apple')
          assert(template.render({:a => 'apples'}), 'apple')
        end
      end

      context('pluralize') do
        template = renderer.parse('{a|plural}')
        should() do
          assert(template.render({:a => 'apple'}), 'apples')
          assert(template.render({:a => 'apples'}), 'apples')
        end
      end
    end

    context('list/dict length') do
      context('without units')
        template = renderer.parse('{a|length}')
        should() do
          assert(template.render({:a => [1,2,1,5,4,3]}), '6')
          assert(template.render({:a => {:q => 1, :w => 2, :e => 3}}), '3')
        end
      end

      context('with units')
        template = renderer.parse("{a|length:item}")
        should() do
          assert(template.render({:a => [1,2,1,5,4,3], :item => {:enUS => 'item'}}) == '6 items')
          assert(template.render({:a => [1], :item => {:enUS => 'item'}}) == '1 item')
        end
      end
    end

    context('default value') do
      template = renderer.parse("{a|default:'none'}")
      should() do
        assert(template.render({:a => 'aa'}), 'aa')
        assert(template.render({:a => nil}), 'none')
      end
    end

    context('simple textile') do
      template = renderer.parse('{a|textile}')
      should('render as emphasied') do
        template.render({:a => '_apple_'}) == '<p><em>apple</em></p>'
      end
      should('render as italics') do
        template.render({:a => '__apple__'}) == '<p><i>apple</i></p>'
      end
      should('render as strong') do
        template.render({:a => '*apple*'}) == '<p><strong>apple</strong></p>'
      end
      should('render as bold') do
        template.render({:a => '**apple**'}) == '<p><b>apple</b></p>'
      end
      should('render as deleted') do
        template.render({:a => '-apple-'}) == '<p><del>apple</del></p>'
      end
      should('render as inserted') do
        template.render({:a => '+apple+'}) == '<p><ins>apple</ins></p>'
      end
      should('render as code') do
        template.render({:a => '@puts("hello")@'}) == '<code>puts("hello")</code>'
      end
      should('render in span') do
        template.render({:a => '%apple%'}) == '<p><span>apple</span></p>'
      end
      should('render superscripted') do
        template.render({:a => '^apple^'}) == '<p><sup>apple</sup></p>'
      end
      should('render subscripted') do
        template.render({:a => ':apple:'}) == '<p><sub>apple</sub></p>'
      end
      should('render citated') do
        template.render({:a => '??apple??'}) == '<p><cit>apple</cit></p>'
      end
    end

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
      should() do
        assert(template.render({:a => 123.4567}) == '123.456')
        assert(template.render({:a => 0.4567}) == '0.456')
        assert(template.render({:a => 0.4}) == '0.4')
      end

      template = renderer.parse('{a|float:4,3}')
      should() do
        template.render({:a => 123.4567}) == '0123.456'
      end
    end

    context('formatting a date') do
      template = renderer.parse('{a|date:dd-MM-yy/hh:mm:ss}')
      should() do
        template.render({:a => ((2008,12,2),(21,26,48))}) == '02-12-2008/21:26:48'
      end
    end

    context('formatting a relative date/time') do
      should() do
        template = renderer.parse('{a|since:now,1}')
        template.render({:a => ((2008,12,2),(21,26,48)), :now => ((2008,12,2),(21,30,48))}) == '5 minutes ago'
      end
      should() do
        template = renderer.parse('{a|until:now,2}')
        template.render({:a => ((2008,12,2),(21,26,48)), :now => ((2008,12,2),(21,22,13))}) == 'in 4 minutes 35 seconds'
      end
    end

    context('formatting a size in bytes') do
      should() do
        template = renderer.parse('{a|since:bytes,2}')
        template.render({:a => 1269.534}) == '1,269.53KB'
      end
      should() do
        template = renderer.parse('{a|since:bytes,2}')
        template.render({:a => 1269.554}) == '1,269.56KB'
      end
    end

    context('formatting a decimal in free form') do
      template = renderer.parse('{a|format:#-###-#######}')
      should() do
        template.render({:a => 88005554567}) == '8-800-555-4567'
      end
    end

    context('formatting a string in free form') do
      template = renderer.parse('{a|format:#-###-#######}')
      should() do
        template.render({:a => '8800call2me'}) == '8-800-call2me'
      end
    end

    context('html escaping') do
      template = renderer.parse('{a|escape}')
      should() do
        template.render({:a => '<h1>"Theory & practice"</h1>'}) == '&lt;h1&gt;&quot;Theory &amp; practice&quot;&lt;/h1&gt;'
      end
    end

    context('html safe') do
      template = renderer.parse('{a|safe}')
      should() do
        template.render({:a => '<h1>Theory</h1>'}) == 'Theory'
      end
    end

    context('text limitation') do
      should() do
        template = renderer.parse('{a|cut:15}')
        template.render({:a => 'a long long long string'}) == 'a long long ...'
      end
      should() do
        template = renderer.parse('{a|cut:30}')
        template.render({:a => 'a long long long string'}) == 'a long long long string'
      end
      should() do
        template = renderer.parse("{a|cut:10,''}")
        template.render({:a => 'a long long long string'}) == 'a long lon'
      end
    end

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
        template.render({:a => ['humour', 'life', 'friends']}) == 'Tags: humour life friends'
      end
    end

    context('list wrap') do
      template = renderer.parse('{a|wrap:(,)}')
      should() do
        template.render({:a => ['humour', 'life', 'friends']}) == '(humour)(life)(friends)'
      end
    end

    context('chaining') do
      context('simple') do
        template = renderer.parse('{a|lower|capital|singular|cut:10}')
        should() do
          template.render({:a => 'appLes'}) == 'Apple'
        end
      end
      context('with parameters') do
        should() do
          template = renderer.parse('{a|wrap:(,)|join:'/'}')
          template.render({:a => ['humour', 'life', 'friends']}) == '(humour)/(life)/(friends)'
        end
        should() do
          template = renderer.parse('{a|split:' ',','|capital|join:/}')
          template.render({:a => 'hello, world'}) == 'Hello/World'
        end
      end
    end

    context('can take variables as parameters') do
      template = renderer.parse('{a|cut:max_length}')
      should() do
        template.render({:a => 'long long long string', :max_length: 10}) == 'long lo...'
      end
    end

    context('can walk through data') do
      context('tuples') do
        template = renderer.parse('{a|sum:(_,x)}')
        should() do
          template.render({:a => [('apples', 11),('bananas', 32)]}) == '43'
        end
      end
      context('dicts') do
        template = renderer.parse('{a|sum:{_,x}}')
        should() do
          template.render({:a => {:apples => 11, :bananas => 32}}) == '43'
        end
      end
      context('lists') do
        template = renderer.parse('{a|sum:[_,x]}')
        should() do
          template.render({:a => [[111, 222], [78, 20]]}) == '242'
        end
      end
    end
  end

  context('i18n') do
    i18n = {
      :apple => {
        :enUS => 'apple',
        :frFr => 'pomme'
      },
      :banana => {
        :enUS => 'banana',
        :frFr => 'banane'
      },
      :price => {
        :enUS => fun(cost, currency) {'Price => {value} {cur}' }
        :frFr => fun(cost, currency) {'Prix => {value} {cur}' }
      } 
    template = renderer.parse('{apples|count::apple}')
    should('default language') do
      template.render({:apples => 3}) == '3 apples'
    end
    should('selected language') do
      template.render({:apples => 3}, 'fr-FR') == '3 pommes'
    end
    template = renderer.parse('{apples|:price}')
    should('lambda based templates') do
      template.render({:value => 5, :cur => 'euro'}, 'en-US') == 'Price: 5 euro'
    end
  end
end
# todo nesting:
# It is possible to call external methods to provide nesting support.
# Imagine we have a template 'home_page', and we want it to consist of several parts:
# 
# My mail
# {nest :mail}
# My calendar
# {nest :calendar}
# My contacts
# {nest :contacts}
# 
# In this case the feedback is called, and it is passed the nested atom and all the parameters.
# With this it is possible to nest templates:
# 
# renderer=Retem()
# template = renderer.parse('...')
# template.render({:apples => apples}) do |nested_template, params| 
#   get_template_by_name(nested_template).render(params) 
# 
# In this case, when retem sees a 'nest' statement, it calls the callback block and inserts
# the result to itself. Of course, nested parts can have nested parts too.


# todo extending filters
# todo preprocessing
