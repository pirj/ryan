module Sessions
  def get(token)
    get(token, erl.ets.lookup(:sessions, token))
  end

  def get(token, [])
    session = Session()
    erl.ets.insert(:sessions, (token, session))
    session
  end

  def get(_token, [(_token2, session)])
    session
  end
end

class Session
  def initialize
    @data = {}
  end

  def has(key)
    @data[key] == nil
  end

  def get(key)
    @data[key]
  end

  def set(key, value)
    d = @data
    d[key] = value
  end

  def inspect()
    @data.to_s()
  end

  def remove(key)
    d = @data
    d[key] = nil
  end
end
