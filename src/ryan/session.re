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

  # def remove(key)
  #   @data = @data.remove(key)
