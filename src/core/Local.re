#
# Local: Methods available locally within any scope
# Copyright (C)2008 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#
module Local
  # Print a string (with newline) to standard output
  def puts(string)
    io::format("~s~n".to_list(), [string.to_list()])
    nil
    
  # Print a string (without newline) to standard output
  def print(string)
    io::format(string.to_list())
    nil
  
  def eval(string)
    Eval.string(string)
    
  def load(path)
    case file::read_file(path.to_list())
      (~ok, data):
        Eval.string(data.to_string())
      error:
        error