module Sessions
  def get(token)
    get(token, ets::lookup(~sessions, token))

  def get(token, [])
    session = Session()
    ets::insert(~sessions, (token, session))
    session

  def get(_token, [(_token2, session)])
    session

class Session
  def initialize
    @data = {}

  def has(key)
    @data[key] == nil

  def get(key)
    @data[key]

  def set(key, value)
    @data = @data.insert(key, value)

  def inspect()
    @data.to_s()

  def remove(key)
    @data = @data.remove(key)
