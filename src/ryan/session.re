class Session
  def initialize
    @data = {}

  def get(key)
    @data[key]

  def set(key, value)
    @data = @data.insert(key, value)

  # def remove(key)
  #   @data = @data.remove(key)
