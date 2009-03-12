class Foo
  def a(x, lambda)
    ['inside a', x].join(' ').puts()
    lambda()
    ['exiting a', x].join(' ').puts()
  end

  def b(y)
    ['inside b', y].join(' ').puts()
  end

  def z
    'z'.puts()
    f = fun do
      b('ccc')
    end
    a('do something', f )
    'exit z'.puts()
  end
end
