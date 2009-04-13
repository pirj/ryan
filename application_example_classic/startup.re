module Startup
  def initialize
    Main.puts('Running example application')
    ets::new(:mail, [:named_table, :public])
    
    inbox = [{:from => 'me', :to => 'John Smith', :contents => 'hi!'}, {:from => 'me', :to => 'Elvis', :contents => 'come back!'}]
    Mailbox.set(:inbox, inbox)
    
    # do something
  end
end

module Mailbox
  def get(box)
    get(box, ets::lookup(:mail, box))
  end
  
  def get(box, [])
    []
  end
  
  def get(box, [(_box, contents)])
    contents
  end
  
  def set(box, contents)
    ets::insert(:mail, (box, contents))
  end
end
