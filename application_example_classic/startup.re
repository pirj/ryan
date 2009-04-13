module Startup
  def initialize
    Main.puts('Running example application')
    ets::new(:mail, [:named_table, :public])
    
    sent = [{:from => 'me', :to => 'John Smith', :contents => 'hi!'}, {:from => 'me', :to => 'Elvis', :contents => 'come back!'}]
    Mailbox.set(:sent, sent)
    
    inbox = [{:to => 'me', :from => 'John Smith', :contents => 'hi back!'}, {:to => 'me', :from => 'Elvis', :contents => 'i`ll be back!'}]
    Mailbox.set(:inbox, inbox)
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
  
  def total
    total = {:unread => get(:unread).size(), :inbox => get(:inbox).size(), :sent => get(:sent).size(), :spam => get(:spam).size(), :trash => get(:trash).size()}
  end
end
