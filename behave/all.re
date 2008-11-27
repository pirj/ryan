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

  context('flow control') do
    context('if statement') do
      template = '{if a}yes{end}'
      should('be resolved') do
        assert(Retem.render(template, {~a: true}), 'yes')
        assert(Retem.render(template, {~a: false}), '')
    
    context('if/else statement') do
      template = '{if a}yes{else}no{end}'
      should('be resolved') do
        assert(Retem.render(template, {~a: true}), 'yes')
        assert(Retem.render(template, {~a: false}), 'no')

    context('if/else/elseif statement') do
      template = '{if a}yes, a{elseif b}yes, b{else}no{end}'
      should('be resolved') do
        assert(Retem.render(template, {~a: true, ~b: true}), 'yes, a')
        assert(Retem.render(template, {~a: false, ~b: true}), 'yes, b')
        assert(Retem.render(template, {~a: false, ~b: false}), 'no')

    context('unless statement') do
      template = '{unless a}no{end}'
      should('be resolved') do
        assert(Retem.render(template, {~a: true}), '')
        assert(Retem.render(template, {~a: false}), 'no')

    context('logical operators') do
      context('(and)') do
        template = '{if a and b}both{end}'
        should('be resolved') do
          assert(Retem.render(template, {~a: true, ~b: true}), 'both')
          assert(Retem.render(template, {~a: true, ~b: false}), '')
          assert(Retem.render(template, {~a: false, ~b: true}), '')
          assert(Retem.render(template, {~a: false, ~b: false}), '')

      context('(or)') do
        template = '{if a or b}yes{end}'
        should('be resolved') do
          assert(Retem.render(template, {~a: true, ~b: true}), 'yes')
          assert(Retem.render(template, {~a: true, ~b: false}), 'yes')
          assert(Retem.render(template, {~a: false, ~b: true}), 'yes')
          assert(Retem.render(template, {~a: false, ~b: false}), '')

    context('comparsion operators') do
      context('(eq)') do
        template = '{if a eq b}equal{end}'
        should('be resolved') do
          assert(Retem.render(template, {~a: 123, ~b: 321}), '')
          assert(Retem.render(template, {~a: 123, ~b: 123}), 'equal')
          assert(Retem.render(template, {~a: true, ~b: false}), '')
          assert(Retem.render(template, {~a: true, ~b: true}), 'equal')
          assert(Retem.render(template, {~a: 'abc', ~b: 'cba'}), '')
          assert(Retem.render(template, {~a: 'abc', ~b: 'abc'}), 'equal')

      context('(neq)') do
        template = '{if a neq b}not equal{end}'
        should('be resolved') do
          assert(Retem.render(template, {~a: 123, ~b: 321}), 'not equal')
          assert(Retem.render(template, {~a: 123, ~b: 123}), '')
          assert(Retem.render(template, {~a: true, ~b: false}), 'not equal')
          assert(Retem.render(template, {~a: true, ~b: true}), '')
          assert(Retem.render(template, {~a: 'abc', ~b: 'cba'}), 'not equal')
          assert(Retem.render(template, {~a: 'abc', ~b: 'abc'}), '')

      context('(gt)') do
        template = '{if a gt b}greather{end}'
        should('be resolved') do
          assert(Retem.render(template, {~a: 123, ~b: 321}), '')
          assert(Retem.render(template, {~a: 123, ~b: 123}), '')
          assert(Retem.render(template, {~a: 321, ~b: 123}), 'greather')
          assert(Retem.render(template, {~a: true, ~b: false}), 'greather')
          assert(Retem.render(template, {~a: true, ~b: true}), '')
          assert(Retem.render(template, {~a: false, ~b: true}), '')
          assert(Retem.render(template, {~a: 'abc', ~b: 'cba'}), '')
          assert(Retem.render(template, {~a: 'abc', ~b: 'abc'}), '')
          assert(Retem.render(template, {~a: 'cba', ~b: 'abc'}), 'greather')

      context('(lt)') do
        template = '{if a lt b}less{end}'
        should('be resolved') do
          assert(Retem.render(template, {~a: 123, ~b: 321}), 'less')
          assert(Retem.render(template, {~a: 123, ~b: 123}), '')
          assert(Retem.render(template, {~a: 321, ~b: 123}), '')
          assert(Retem.render(template, {~a: true, ~b: false}), '')
          assert(Retem.render(template, {~a: true, ~b: true}), '')
          assert(Retem.render(template, {~a: false, ~b: true}), 'less')
          assert(Retem.render(template, {~a: 'abc', ~b: 'cba'}), 'less')
          assert(Retem.render(template, {~a: 'abc', ~b: 'abc'}), '')
          assert(Retem.render(template, {~a: 'cba', ~b: 'abc'}), '')

      context('(gteq)') do
        template = '{if a gteq b}greather or eq{end}'
        should('be resolved') do
          assert(Retem.render(template, {~a: 123, ~b: 321}), '')
          assert(Retem.render(template, {~a: 123, ~b: 123}), 'greather or eq')
          assert(Retem.render(template, {~a: 321, ~b: 123}), 'greather or eq')
          assert(Retem.render(template, {~a: true, ~b: false}), 'greather or eq')
          assert(Retem.render(template, {~a: true, ~b: true}), 'greather or eq')
          assert(Retem.render(template, {~a: false, ~b: true}), '')
          assert(Retem.render(template, {~a: 'abc', ~b: 'cba'}), '')
          assert(Retem.render(template, {~a: 'abc', ~b: 'abc'}), 'greather or eq')
          assert(Retem.render(template, {~a: 'cba', ~b: 'abc'}), 'greather or eq')

      context('(lteq)') do
        template = '{if a lteq b}less or eq{end}'
        should('be resolved') do
          assert(Retem.render(template, {~a: 123, ~b: 321}), 'less or eq')
          assert(Retem.render(template, {~a: 123, ~b: 123}), 'less or eq')
          assert(Retem.render(template, {~a: 321, ~b: 123}), '')
          assert(Retem.render(template, {~a: true, ~b: false}), '')
          assert(Retem.render(template, {~a: true, ~b: true}), 'less or eq')
          assert(Retem.render(template, {~a: false, ~b: true}), 'less or eq')
          assert(Retem.render(template, {~a: 'abc', ~b: 'cba'}), 'less or eq')
          assert(Retem.render(template, {~a: 'abc', ~b: 'abc'}), 'less or eq')
          assert(Retem.render(template, {~a: 'cba', ~b: 'abc'}), '')

# todo flow control:
# if
# {if a > 123}yes{end}
# {if a gt 3 or b lt 10}...{end}
# {if a or b and c}...{end}


# todo loops

# todo filtering

# todo i18n

# todo nesting

# todo extending filters