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

  Scenario: Mark ruby expand up 1 level
    Given I turn on ruby-mode
    And there is no region selected
    When I insert:
    """
    #comment foo
    module Bar
      def foo
        bar
      end
    end

    """
    And I go to line "3"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    Then the region should be:
    """
    module Bar
      def foo
        bar
      end
    end

    """

  Scenario: Mark ruby expand up 3 levels
    Given I turn on ruby-mode
    And there is no region selected
    When I insert:
    """
    #comment foo
    module Bar

      attr_reader :blah

      foo_arr.each do |element|
        blah {
          puts something
        }
      end

      def foo
        bar
      end
    end

    """
    And I go to line "7"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    Then the region should be:
    """
    module Bar

      attr_reader :blah

      foo_arr.each do |element|
        blah {
          puts something
        }
      end

      def foo
        bar
      end
    end

    """

  Scenario: Mark ruby expand to whole buffer
    Given I turn on ruby-mode
    And there is no region selected
    When I insert:
    """
    class Foo
      def blah
        [1,2,3].each do |num|
          puts num
        end
      end
    end

    #comment foo
    module Bar
      def foo
        bar
      end
    end

    """
    And I go to line "12"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    Then the region should be:
    """
    class Foo
      def blah
        [1,2,3].each do |num|
          puts num
        end
      end
    end

    #comment foo
    module Bar
      def foo
        bar
      end
    end

    """
