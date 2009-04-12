Behave.context('Templating') do
  Behave.context('text') do
    Behave.should('to_string test') do
      :'foo bar'.to_s() == "foo bar"
    end
    Behave.should('to_string test') do
      :'foo bar'.inspect() == ":'foo bar'"
    end
    Behave.should('to_string test') do
      :'foo bars'.inspect() == ":'foo bar'"
    end
    Behave.context('text2') do
      Behave.should('to_string test') do
        :'foo bar'.to_s() == "foo bar"
      end
    end
  end
end