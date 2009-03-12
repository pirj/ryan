# Templating with retem

Retem is a templating engine which comes with Ryan web framework for Reia programming
language.
It provides convenient syntax and a lot if features available out of the box:

 - variables

 - flow control

 - filters

 - i18n

 - nesting of templates

## Usage

template = """Total {apples|count:apples}
{for apple in apples} {apple.color} {apple.weight}kg {end}"""
apples = [{:color => 'red', :weight => 0.2}, {:color => 'yellow', :weight => 0.15}]
renderer = Retem()
renderer.parse(template).render({:apples => apples})
=> Total 2 apples
.. red 0.2kg
.. yellow 0.15kg

## Flow control

The simplest branching construct available is the if statement, which takes the specified branch if the condition holds true. The else keyword specifies an action to be taken if the condition doesn't match.
Multiple if statements can be chained with the elseif keyword. Else and elseif are optional to use.

{if condition1}
  ...
{elseif condition2}
  ...
{elseif condition3}
  ...
{else}
  ...
{end}

The unless keyword functions like the if keyword, except expression matching is inverted and the branch is taken if the condition does not hold.

{unless condition}
  ...
{end}

Condition can consist of multiple conditions:

{if condition1 and condition2}...{end}
{if condition1 or condition2}...{end}

Condition can be a result of comparsion:
{if bees_count gt flowers_count}...{end}

The following comparsion operators can be used:

 - eq    equal to

 - neq   not equal to

 - gt    greater than

 - lt    less than

 - gteq  greater than or equal to

 - lteq  less than or equal to

It is possible to use these kind of constructions:
{if a gt 3 or b lt 10}...{end}


For loops can iterate lists, tuples and dicts:

mylist=[1, 2, 3]
{for x in mylist} {x*2} {end}
=>  2  4  6 

mytuple=(1, 2, 3, 5)
{for x in mytuple} {x*2} {end}
=>  2  4  6  10 

mydict={'a': 3, 'b': 4}
{for {k:v} in mydict} {k}:{v*2} {end}
=> a:6  b:8 

Pattern matching can be used when iterating lists of objects.
When iterating a list of lists:
mylist = [['aaa', 123], ['qqq', 456]]
{for [a,b] in mylist}
  {a} {b}
{end}
=>  aaa 123  qqq 456

When iterating a list of tuples:
mylist = [('aaa', 123, 'www'), ('qqq', 456, 'zzz')]
{for (a,b,c) in mylist}
  {a} {b} {c}
{end}
=>  aaa 123 www  qqq 456 zzz

When iterating a list of dicts/objects:
apples = [{:color => 'red', :weight => 0.2}, {:color => 'yellow', :weight => 0.15}]
{for {:color => color, :weight => weight} in apples}
  Color is {color} and weight is {weight}
{end}
=> Color is red and weight is 0.2  Color is yellow and weight is 0.15

## Access to variables

Variables passed to retem can be freely used in templates:

{apple}

{apple.color}

{store.address.city}

## Filtering

Filtering allows for value modification, formatting etc. Filter signature comes after '|' character.
Filter parameters are passed after ':' character. Multiple parameters can be passed using ',' character.
For those parameters that can be treaten erratic, single quoting can be used.

The following examples are self-explaining, taking into account that 'a' is a variable being passed
to a template in curly braces:

a='apPles'
{a|capital}
=> ApPles

a='apPles AnD BaNaNas'
{a|capitalphrase}
=> Apples and bananas

a='apPles'
{a|lower}
=> apples

a='apPles'
{a|upper}
=> APPLES

a='app_les'
{a|pretty}
=> App les

a='apple'
{a|plural}
=> apples

a='apples'
{a|singular}
=> apple

a='apples'
{a|length}
=> 6

a=nil
{a|default:'none'}
=> none

a='*apple*'
{a|textile}
=> <b>apple</b>

a='/apple/'
{a|textile}
=> <i>apple</i>

a=123
{a|float:0,3}
=> 123.000

a=123
{a|float:4,3}
=> 0123.000

a=Time.now
{a|date:dd-MM-yy/hh:mm:ss}
=> 11-11-2008/02:31:14

a=Time.now - 100
{a|since:1}
=> 2 minutes

a=Time.now + 100
{a|until:2}
=> 1 minute, 40 seconds

a=1300000
{a|bytes:2}
=> 1,269.53KB

a=88005554567
{a|format:#-###-###-####}
=> 8-800-555-4567

a='8800call2me'
{a|format:#-###-#######}
=> 8-800-call2me

a='<h1>"Theory & practice"</h1>'
{a|escape}
=> &lt;h1&gt;&quot;Theory &amp; practice&quot;&lt;/h1&gt;

a='<h1>Theory</h1>'
{a|safe}
=> Theory

a='a long long long string'
{a|cut:15}
=> a long long ...
{a|cut:30}
=> a long long long string

a="""a long
long
string"""
{a|lines:0}
0 a long
1 long
2 string

a=['apple', 'banana', 'coconut']
{a|count}
=> 3

a=['humour', 'life', 'friends']
Tags: {a|join:' '}
=> Tags: humour life friends

Filters can take several parameters:
a=['humour', 'life', 'friends']
{a|wrap:(,)}
=> (humour)(life)(friends)

{a|cut:10,''}
=> a long lon

a=[123, 345, 567]
{a|count:'item','items'}
=> 3 items

a=[123, 345, 567]
- kilo !countable
  en-US: kilogram, kilograms
  fr-FR: kilogramme, kilogrammes
{a|count:kilo}
=> 3 kilograms

Filters can be chained:
a='appLes'
{a|lower|capital|singular|cut:10}
=> Apple

a=['humour', 'life', 'friends']
{a|wrap:(,)|join:'/'}
=> (humour)/(life)/(friends)

a='hello, world'
{a|wrap:*|textile}
=> <b>hello, world</b>

a='hello, world'
{a|split:' ',','|capital|join:/}
=> Hello/World

Filters can take variables as parameters:
a='long long long string'
max_length=10
{a|cut:max_length}
=> long lo...

Filters can walk through data:
a=[('apples', 11),('bananas', 32)]
{a|sum:(_,x)}
=> 43

a=[{:apples => 11},{:bananas => 32}]
{a|sum:{_,x}}
=> 43

a=[[111, 222], [78, 20]]
{a|sum:[_,x]}
=> 242

## Extending filters with native code

It is possible to implement additional filters:

src/behave/child_filter.re:
module ChildFilter
  def filter(string)
     if string == 'shit'
       '****'

renderer = Retem()
renderer.add_filter(ChildFilter)
template = renderer.parse('{a|child_filter}')
template.render({a => 'holy shit}) # holy ****

## i18n
It is possible to provide custom translations dictionary in the following format:
apps/my_app/i18n.re:
i18n = {
  apple => {
    :enUS => 'apple',
    :frFr => 'pomme'
  },
  :banana: {
    :enUS => 'banana',
    :frFr => 'banane'
  },
  :price: {
    :enUS => fun(cost, currency) {"Price: #{value} #{cur}" }
    :frFr => fun(cost, currency) {"Prix: #{value} #{cur}" }
  } 

Retem uses 'en-US' as default unless otherwise requested.
Dictionary is added to renderer instance:

renderer = Retem()

template = renderer.parse('{apples|count:apples}')
template.render({:apples => 3})
=> 3 apples
template.render('{apples|count:apples}', {:apples => 3}, 'fr-FR')
=> 3 pommes

## Nesting

It is possible to call external methods to provide nesting support.
Imagine we have a template 'home_page', and we want it to consist of several parts:

My mail
{nest :mail}
My calendar
{nest :calendar}
My contacts
{nest :contacts}

In this case the feedback is called, and it is passed the nested atom and all the parameters.
With this it is possible to nest templates:

renderer=Retem()
template = renderer.parse('...')
template.render({:apples => apples}) do |nested_template, params| 
  get_template_by_name(nested_template).render(params) 

In this case, when retem sees a 'nest' statement, it calls the callback block and inserts
the result to itself. Of course, nested parts can have nested parts too.

## Preprocessing

To skip rendering of the code pieces, that are render-to-render static, preprocessing
can be used.

renderer=Retem()
template = renderer.parse("Server's ip address is {{server_ip}}, user's ip address is {user_ip}", {:server_ip => '55.55.55.55'})
template.render({:user_ip => '111.111.111.111'})
=> Server's ip address is '55.55.55.55', user's ip address is 111.111.111.111

During render, only user_ip is evaluated.

### Authors
* Philpipp Pirozhkov pirj@mail.ru
