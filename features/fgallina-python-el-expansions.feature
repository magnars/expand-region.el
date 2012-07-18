Feature: fgallinas python.el expansions
  In order to quickly and precisely mark Python code blocks
  As an Emacs user
  I want to expand to them

  Scenario: Baseline feature test.
    Given I turn on python-mode
    And there is no region selected
    When I insert "run(23)"
    And I place the cursor between "n" and "("
    And I press "C-@"
    And I press "C-@"
    Then the region should be "run(23)"

  Scenario: Mark region inside a string.
    Given I turn on python-mode
    And there is no region selected
    When I insert "'X-Men: Wolverine'"
    And I place the cursor between "r" and "i"
    And I press "C-@"
    And I press "C-@"
    Then the region should be "X-Men: Wolverine"

  Scenario: Mark region outside a string.
    Given I turn on python-mode
    And there is no region selected
    When I insert "run('X-Men: ' + 'Wolverine')"
    And I place the cursor between "M" and "e"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    Then the region should be "'X-Men: '"

  Scenario: Mark region inside a multi-line string.
    Given I turn on python-mode
    And there is no region selected
    When I insert:
      """
      print('lalelu')

      '''This is a multi-line Python string
      with lots of useless content.
      '''

      print('lalelu')
      """
    And I place the cursor between "-" and "l"
    And I press "C-@"
    And I press "C-@"
    Then the region should be:
      """
      This is a multi-line Python string
      with lots of useless content.

      """

  Scenario: Mark region outside a multi-line string.
    Given I turn on python-mode
    And there is no region selected
    When I insert:
      """
      '''This is a multi-line Python string
      with lots of useless content.
      '''
      """
    And I place the cursor between "-" and "l"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    Then the region should be:
      """
      '''This is a multi-line Python string
      with lots of useless content.
      '''
      """

  Scenario: Mark a basic Python block
    Given I turn on python-mode
    And there is no region selected
    When I insert:
      """
      if True:
          print('To be, or not to be...')
      else:
          print('Booyah.')
      """
    And I go to point "1"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    Then the region should be:
      """
      if True:
          print('To be, or not to be...')
      """

  Scenario: Mark a Python block with a nested block
    Given I turn on python-mode
    And there is no region selected
    When I insert:
      """
      if True:
          if True:
              print(23)
          print('To be, or not to be...')
      else:
          print('Booyah.')
      """
    And I go to point "1"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    Then the region should be:
      """
      if True:
          if True:
              print(23)
          print('To be, or not to be...')
      """

  Scenario: Mark an outer Python block
    Given I turn on python-mode
    And there is no region selected
    When I insert:
      """
      print('More stuff')

      def the_truth():
          if True:
              print('To be, or not to be...')
          else:
              print('Booyah.')

      print('Even more stuff.')
      """
    And I go to point "42"
    And I press "C-@"
    Then the region should be:
      """
      if
      """
    And I press "C-@"
    Then the region should be:
      """
      if True:
      """
    And I press "C-@"
    Then the region should be:
      """
      if True:
              print('To be, or not to be...')
      """
    And I press "C-@"
    Then the region should be:
      """
      def the_truth():
          if True:
              print('To be, or not to be...')
          else:
              print('Booyah.')
      """
