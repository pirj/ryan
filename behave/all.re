Behave.context('Templating') do

  context('text') do
    template = 'la 123%5442!@#$%^&*()|":LK<MN~123efgsfd"'
    should('be resolved with no changes') do
	  Retem.render(template, {~a: 100}) == template
    
  context('empty braces') do
    should('be resolved to an empty string') do
	  assert(Retem.render('{}'), '')
	  assert(Retem.render('{ }'), '')

  context('variable') do
    should('be resolved (numeric)') do
	  Retem.render('{a}', {~a: 100}) == '100'
    should('be resolved (alpha)') do
	  Retem.render('{a}', {~a: 'abc'}) == 'abc'

  context('variable referencing a variable') do
    should('be resolved') do
	  Retem.render('{a}', {~a: ~b, ~b: 'abc'}) == 'abc'
	
  context('flow control')
    context('if/else statement')
      template = '{if a}yes{else}no{end}'
      should('be resolved') do
	    assert(Retem.render(template, {~a: true}), 'yes')
	    assert(Retem.render(template, {~a: false}), 'no')

# todo flow control:
# if
# * eq    equal to
# * neq   not equal to
# * gt    greater than
# * lt    less than
# * gteq  greater than or equal to
# * lteq  less than or equal to

# todo loops

# todo filtering

# todo i18n

# todo nesting

# todo extending filters