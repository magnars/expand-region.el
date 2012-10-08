Feature: ruby-mode expansions
  In order to quickly and precisely mark ruby code blocks
  As an Emacs user
  I want to expand to them

  Scenario: Mark ruby block
    Given I turn on ruby-mode
    And there is no region selected
    When I insert:
    """
    module Bar
      something do
        foo
      end
    end
    """
    And I place the cursor after "something"
    And I press "C-@"
    And I press "C-@"
    Then the region should be:
    """
    something do
        foo
      end
    """

  Scenario: Mark ruby block from end
    Given I turn on ruby-mode
    And there is no region selected
    When I insert:
    """
    module Bar
      something do
        foo
      end
    end
    """
    And I place the cursor after "end"
    And I press "C-@"
    And I press "C-@"
    Then the region should be:
    """
    something do
        foo
      end
    """

  Scenario: Mark ruby block from within
    Given I turn on ruby-mode
    And there is no region selected
    When I insert:
    """
    module Bar
      something do
        foo
      end
    end
    """
    And I go to line "2"
    And I press "C-@"
    And I press "C-@"
    Then the region should be:
    """
    something do
        foo
      end
    """

  Scenario: Mark ruby block with using curly brackets
    Given I turn on ruby-mode
    And there is no region selected
    When I insert:
    """
    module Bar
      something {
        foo
      }
    end
    """
    And I go to line "3"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    Then the region should be:
    """
    something {
        foo
      }
    """

  Scenario: Mark ruby function at the beginning
    Given I turn on ruby-mode
    And there is no region selected
    When I insert:
    """
    module Bar
      def foo
        bar
      end
    end
    """
    And I go to word "def"
    And I press "C-@"
    And I press "C-@"
    Then the region should be:
    """
    def foo
        bar
      end
    """

  Scenario: Mark ruby function at definition
    Given I turn on ruby-mode
    And there is no region selected
    When I insert:
    """
    module Bar
      def foo
        bar
      end
    end
    """
    And I go to line "3"
    And I press "C-@"
    And I press "C-@"
    Then the region should be:
    """
    def foo
        bar
      end
    """
