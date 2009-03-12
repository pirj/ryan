module Sessions
  def get(token)
    get(token, ets::lookup(:sessions, token))
  end

  def get(token, [])
    session = Session()
    ets::insert(:sessions, (token, session))
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
    @data = @data.insert(key, value)
  end

  def inspect()
    @data.to_s()
  end

  def remove(key)
    @data = @data.remove(key)
  end
end
