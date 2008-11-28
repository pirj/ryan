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

# todo loops

# todo filtering

# todo i18n

# todo nesting

# todo extending filters