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

  Behave.context('text') do
    string = 'la 123%5442!@#$%^&*()|":LK<MN~123efgsfd"'
    template = renderer.parse(string)
    Behave.should('be resolved with no changes') do
      template.render({:a => 100}) == string
    end
  end

  Behave.context('empty braces') do
    Behave.should('be resolved to an empty string') do
      assert(renderer.parse('{}').render() == '')
      assert(renderer.parse('{ }').render() == '')
    end
  end

  Behave.context('variable') do
    Behave.should('be resolved (numeric)') do
      Retem.render('{a}', {:a => 100}) == '100'
    end
    Behave.should('be resolved (alpha)') do
      Retem.render('{a}', {:a => 'abc'}) == 'abc'
    end
  end

  Behave.context('variable referencing a variable') do
    Behave.should() do
      Retem.render('{a}', {:a => :b, :b => 'abc'}) == 'abc'
    end
  end

  Behave.context('variable properties') do
    address = Address()
    address.set_street('main st.')
    store = Store()
    store.set_address(address)
    store.set_name('store name')
    Behave.should() do
      Retem.render('{s.name}', {:s => store}) == 'store name'
    end
    Behave.should('be deep resolved') do
      Retem.render('{s.address.street}', {:s => store}) == 'main st.'
    end
  end

  Behave.context('flow control') do
    Behave.context('if statement') do
      template = renderer.parse('{if a}yes{end}')
      Behave.should() do
        assert(template.render({:a => true}) == 'yes')
        assert(template.render({:a => false}) == '')
      end
    end

    Behave.context('if/else statement') do
      template = renderer.parse('{if a}yes{else}no{end}')
      Behave.should() do
        assert(template.render({:a => true}) == 'yes')
        assert(template.render({:a => false}) == 'no')
      end
    end

    Behave.context('if/else/elseif statement') do
      template = renderer.parse('{if a}yes, a{elseif b}yes, b{else}no{end}')
      Behave.should() do
        assert(template.render({:a => true, :b => true}) == 'yes, a')
        assert(template.render({:a => false, :b => true}) == 'yes, b')
        assert(template.render({:a => false, :b => false}) == 'no')
      end
    end

    Behave.context('unless statement') do
      template = renderer.parse('{unless a}no{end}')
      Behave.should() do
        assert(template.render({:a => true}) == '')
        assert(template.render({:a => false}) == 'no')
      end
    end

    Behave.context('logical operators') do
      Behave.context('(and)') do
        template = renderer.parse('{if a and b}both{end}')
        Behave.should() do
          assert(template.render({:a => true, :b => true}) == 'both')
          assert(template.render({:a => true, :b => false}) == '')
          assert(template.render({:a => false, :b => true}) == '')
          assert(template.render({:a => false, :b => false}) == '')
        end
      end

      Behave.context('(or)') do
        template = renderer.parse('{if a or b}yes{end}')
        Behave.should() do
          assert(template.render({:a => true, :b => true}) == 'yes')
          assert(template.render({:a => true, :b => false}) == 'yes')
          assert(template.render({:a => false, :b => true}) == 'yes')
          assert(template.render({:a => false, :b => false}) == '')
        end
      end
    end

    Behave.context('comparsion operators') do
      Behave.context('(eq)') do
        template = renderer.parse('{if a eq b}equal{end}')
        Behave.should() do
          assert(template.render({:a => 123, :b => 321}) == '')
          assert(template.render({:a => 123, :b => 123}) == 'equal')
          assert(template.render({:a => true, :b => false}) == '')
          assert(template.render({:a => true, :b => true}) == 'equal')
          assert(template.render({:a => 'abc', :b => 'cba'}) == '')
          assert(template.render({:a => 'abc', :b => 'abc'}) == 'equal')
        end
      end

      Behave.context('(neq)') do
        template = renderer.parse('{if a neq b}not equal{end}')
        Behave.should() do
          assert(template.render({:a => 123, :b => 321}) == 'not equal')
          assert(template.render({:a => 123, :b => 123}) == '')
          assert(template.render({:a => true, :b => false}) == 'not equal')
          assert(template.render({:a => true, :b => true}) == '')
          assert(template.render({:a => 'abc', :b => 'cba'}) == 'not equal')
          assert(template.render({:a => 'abc', :b => 'abc'}) == '')
        end
      end

      Behave.context('(gt)') do
        template = renderer.parse('{if a gt b}greather{end}')
        Behave.should() do
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

      Behave.context('(lt)') do
        template = renderer.parse('{if a lt b}less{end}')
        Behave.should() do
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

      Behave.context('(gteq)') do
        template = renderer.parse('{if a gteq b}greather or eq{end}')
        Behave.should() do
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

      Behave.context('(lteq)') do
        template = renderer.parse('{if a lteq b}less or eq{end}')
        Behave.should() do
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

    Behave.context('mixed non-variable/variable comparsions') do
      Behave.context('numeric') do
        template = renderer.parse('{if a gt 132}greather{end}')
        Behave.should() do
          assert(template.render({:a => 123}) == '')
          assert(template.render({:a => 321}) == 'greather')
        end
      end

      Behave.context('boolean') do
        template = renderer.parse('{if a gt false}greather{end}')
        Behave.should() do
          assert(template.render({:a => true}) == 'greather')
          assert(template.render({:a => false}) == '')
        end
      end

      Behave.context('string') do
        template = renderer.parse('{if a gt "abc"}greather{end}')
        Behave.should() do
          assert(template.render({:a => 'abc'}) == '')
          assert(template.render({:a => 'cba'}) == 'greather')
        end
      end
    end

    Behave.context('composite comparsions') do
      Behave.context('chained') do
        template = renderer.parse('{if a or b and c}a or b and c{end}')
        Behave.should() do
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

      Behave.context('compound') do
        template = renderer.parse('{if a gt 3 and b lt 3}a > 3 and b < 3{end}')
        Behave.should() do
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

  Behave.context('iterating through') do
    Behave.context('a list') do
      template = renderer.parse('{for x in many}{x}{end}')
      Behave.should() do
        assert(template.render({:many => [1, 2, 3]}) == '123')
        assert(template.render({:many => ['a', 'b', 'c']}) == 'abc')
      end
    end

    Behave.context('a tuple') do
      template = renderer.parse('{for x in many}{x}{end}')
      Behave.should() do
        assert(template.render({:many => (1, 2, 3)}) == '123')
        assert(template.render({:many => ('a', 'b', 'c')}) == 'abc')
      end
    end

    Behave.context('a dict') do
      template = renderer.parse('{for {k:v} in many}-{k}:{v}{end}')
      Behave.should() do
        assert(template.render({:many => {:a => 3, :b => 4}}) == '-a:3-b:4')
        assert(template.render({:many => {'a' => 3, 'b' => 4}}) == '-a:3-b:4')
        assert(template.render({:many => {'a' => 'q', 'b' => 'w'}}) == '-a:q-b:w')
      end
    end
  end

  Behave.context('iterating with pattern matching through') do
    Behave.context('a list') do
      template = renderer.parse('{for [a,b] in many}-{a}:{b}{end}')
      Behave.should() do
        template.render({:many => [['aaa', 123], ['qqq', 456]]}) == '-aaa:123-qqq:456'
      end
    end

    Behave.context('a list of tuples') do
      template = renderer.parse('{for (a,b,c) in many}-{a}:{b}:{c}{end}')
      Behave.should() do
        template.render({:many => [('aaa', 123, 'www'), ('qqq', 456, 'zzz')]}) == '-aaa:123:www-qqq:456:zzz'
      end
    end

    Behave.context('a list of dicts/objects') do
      template = renderer.parse('{for {color: :color, weight: :weight} in apples}-{color}:{weight}{end}')
      Behave.should() do
        template.render({:apples => [{:color => 'red', :weight => 0.2}, {:color => 'yellow', :weight => 0.15}]}) == '-red:0.2-yellow:0.15'
      end
    end
  end

  Behave.context('filtering') do
    Behave.context('capital') do
      template = renderer.parse('{a|capital}')
      Behave.should('be resolved (first letter only)') do
        template.render({:a => 'apPles'}) == 'ApPles'
      end

      template = renderer.parse('{a|capitalwords}')
      Behave.should('words') do
        template.render({:a => 'apPles AnD BaNaNas'}) == 'Apples And Bananas'
      end

      template = renderer.parse('{a|capitalphrase}')
      Behave.should('whole phrase') do
        template.render({:a => 'apPles AnD BaNaNas'}) == 'Apples and bananas'
      end
    end

    Behave.context('lower case') do
      template = renderer.parse('{a|lower}')
      Behave.should() do
        template.render({:a => 'apPles'}) == 'apples'
      end
    end

    Behave.context('upper case') do
      template = renderer.parse('{a|upper}')
      Behave.should() do
        template.render({:a => 'apPles'}) == 'APPLES'
      end
    end

    Behave.context('pretty filter') do
      template = renderer.parse('{a|pretty}')
      Behave.should() do
        template.render({:a => 'app_les'}) == 'App les'
      end
    end

    Behave.context('inflection') do
      Behave.context('singularize') do
        template = renderer.parse('{a|singular}')
        Behave.should() do
          assert(template.render({:a => 'apple'}), 'apple')
          assert(template.render({:a => 'apples'}), 'apple')
        end
      end

      Behave.context('pluralize') do
        template = renderer.parse('{a|plural}')
        Behave.should() do
          assert(template.render({:a => 'apple'}), 'apples')
          assert(template.render({:a => 'apples'}), 'apples')
        end
      end
    end

    Behave.context('list/dict length') do
      Behave.context('without units')
        template = renderer.parse('{a|length}')
        Behave.should() do
          assert(template.render({:a => [1,2,1,5,4,3]}), '6')
          assert(template.render({:a => {:q => 1, :w => 2, :e => 3}}), '3')
        end
      end

      Behave.context('with units')
        template = renderer.parse("{a|length:item}")
        Behave.should() do
          assert(template.render({:a => [1,2,1,5,4,3], :item => {:enUS => 'item'}}) == '6 items')
          assert(template.render({:a => [1], :item => {:enUS => 'item'}}) == '1 item')
        end
      end
    end

    Behave.context('default value') do
      template = renderer.parse("{a|default:'none'}")
      Behave.should() do
        assert(template.render({:a => 'aa'}), 'aa')
        assert(template.render({:a => nil}), 'none')
      end
    end

    Behave.context('simple textile') do
      template = renderer.parse('{a|textile}')
      Behave.should('render as emphasied') do
        template.render({:a => '_apple_'}) == '<p><em>apple</em></p>'
      end
      Behave.should('render as italics') do
        template.render({:a => '__apple__'}) == '<p><i>apple</i></p>'
      end
      Behave.should('render as strong') do
        template.render({:a => '*apple*'}) == '<p><strong>apple</strong></p>'
      end
      Behave.should('render as bold') do
        template.render({:a => '**apple**'}) == '<p><b>apple</b></p>'
      end
      Behave.should('render as deleted') do
        template.render({:a => '-apple-'}) == '<p><del>apple</del></p>'
      end
      Behave.should('render as inserted') do
        template.render({:a => '+apple+'}) == '<p><ins>apple</ins></p>'
      end
      Behave.should('render as code') do
        template.render({:a => '@puts("hello")@'}) == '<code>puts("hello")</code>'
      end
      Behave.should('render in span') do
        template.render({:a => '%apple%'}) == '<p><span>apple</span></p>'
      end
      Behave.should('render superscripted') do
        template.render({:a => '^apple^'}) == '<p><sup>apple</sup></p>'
      end
      Behave.should('render subscripted') do
        template.render({:a => ':apple:'}) == '<p><sub>apple</sub></p>'
      end
      Behave.should('render citated') do
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

    Behave.context('formatting a float number') do
      template = renderer.parse('{a|float:,3}')
      Behave.should() do
        assert(template.render({:a => 123.4567}) == '123.456')
        assert(template.render({:a => 0.4567}) == '0.456')
        assert(template.render({:a => 0.4}) == '0.4')
      end

      template = renderer.parse('{a|float:4,3}')
      Behave.should() do
        template.render({:a => 123.4567}) == '0123.456'
      end
    end

    Behave.context('formatting a date') do
      template = renderer.parse('{a|date:dd-MM-yy/hh:mm:ss}')
      Behave.should() do
        template.render({:a => ((2008,12,2),(21,26,48))}) == '02-12-2008/21:26:48'
      end
    end

    Behave.context('formatting a relative date/time') do
      Behave.should() do
        template = renderer.parse('{a|since:now,1}')
        template.render({:a => ((2008,12,2),(21,26,48)), :now => ((2008,12,2),(21,30,48))}) == '5 minutes ago'
      end
      Behave.should() do
        template = renderer.parse('{a|until:now,2}')
        template.render({:a => ((2008,12,2),(21,26,48)), :now => ((2008,12,2),(21,22,13))}) == 'in 4 minutes 35 seconds'
      end
    end

    Behave.context('formatting a size in bytes') do
      Behave.should() do
        template = renderer.parse('{a|since:bytes,2}')
        template.render({:a => 1269.534}) == '1,269.53KB'
      end
      Behave.should() do
        template = renderer.parse('{a|since:bytes,2}')
        template.render({:a => 1269.554}) == '1,269.56KB'
      end
    end

    Behave.context('formatting a decimal in free form') do
      template = renderer.parse('{a|format:#-###-#######}')
      Behave.should() do
        template.render({:a => 88005554567}) == '8-800-555-4567'
      end
    end

    Behave.context('formatting a string in free form') do
      template = renderer.parse('{a|format:#-###-#######}')
      Behave.should() do
        template.render({:a => '8800call2me'}) == '8-800-call2me'
      end
    end

    Behave.context('html escaping') do
      template = renderer.parse('{a|escape}')
      Behave.should() do
        template.render({:a => '<h1>"Theory & practice"</h1>'}) == '&lt;h1&gt;&quot;Theory &amp; practice&quot;&lt;/h1&gt;'
      end
    end

    Behave.context('html safe') do
      template = renderer.parse('{a|safe}')
      Behave.should() do
        template.render({:a => '<h1>Theory</h1>'}) == 'Theory'
      end
    end

    Behave.context('text limitation') do
      Behave.should() do
        template = renderer.parse('{a|cut:15}')
        template.render({:a => 'a long long long string'}) == 'a long long ...'
      end
      Behave.should() do
        template = renderer.parse('{a|cut:30}')
        template.render({:a => 'a long long long string'}) == 'a long long long string'
      end
      Behave.should() do
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

    Behave.context('list join') do
      template = renderer.parse("Tags: {a|join:' '}")
      Behave.should() do
        template.render({:a => ['humour', 'life', 'friends']}) == 'Tags: humour life friends'
      end
    end

    Behave.context('list wrap') do
      template = renderer.parse('{a|wrap:(,)}')
      Behave.should() do
        template.render({:a => ['humour', 'life', 'friends']}) == '(humour)(life)(friends)'
      end
    end

    Behave.context('chaining') do
      Behave.context('simple') do
        template = renderer.parse('{a|lower|capital|singular|cut:10}')
        Behave.should() do
          template.render({:a => 'appLes'}) == 'Apple'
        end
      end
      Behave.context('with parameters') do
        Behave.should() do
          template = renderer.parse("{a|wrap:(,)|join:'/'}")
          template.render({:a => ['humour', 'life', 'friends']}) == '(humour)/(life)/(friends)'
        end
        Behave.should() do
          template = renderer.parse("{a|split:', '|capital|join:/}")
          template.render({:a => 'hello, world'}) == 'Hello/World'
        end
      end
    end

    Behave.context('can take variables as parameters') do
      template = renderer.parse('{a|cut:max_length}')
      Behave.should() do
        template.render({:a => 'long long long string', :max_length => 10}) == 'long lo...'
      end
    end

    Behave.context('can walk through data') do
      Behave.context('tuples') do
        template = renderer.parse('{a|sum:(_,x)}')
        Behave.should() do
          template.render({:a => [('apples', 11),('bananas', 32)]}) == '43'
        end
      end
      Behave.context('dicts') do
        template = renderer.parse('{a|sum:{_,x}}')
        Behave.should() do
          template.render({:a => {:apples => 11, :bananas => 32}}) == '43'
        end
      end
      Behave.context('lists') do
        template = renderer.parse('{a|sum:[_,x]}')
        Behave.should() do
          template.render({:a => [[111, 222], [78, 20]]}) == '242'
        end
      end
    end
  end

  Behave.context('i18n') do
    apple = {:enUS => 'apple', :frFr => 'pomme'}
    banana = {:enUS => 'banana', :frFr => 'banane'}
    price = {:enUS => fun(cost, currency) {'Price => {value} {cur}' }, :frFr => fun(cost, currency) {'Prix => {value} {cur}'}}
    i18n = {:apple => apple, :banana => banana, :price => price}
    template = renderer.parse('{apples|count::apple}')
    Behave.should('default language') do
      template.render({:apples => 3}) == '3 apples'
    end
    Behave.should('selected language') do
      template.render({:apples => 3}, 'fr-FR') == '3 pommes'
    end
    template = renderer.parse('{apples|:price}')
    Behave.should('lambda based templates') do
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
