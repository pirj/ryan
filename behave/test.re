class Foo
  def a(x, lambda)
    ['inside a', x].join(' ').puts()
    lambda()
    ['exiting a', x].join(' ').puts()

  def b(y)
    ['inside b', y].join(' ').puts()

  def z
    'z'.puts()
    a('do something', fun do
      b('ccc')
    )
    'exit z'.puts()
