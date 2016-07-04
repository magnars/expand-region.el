@requires-e24-3
Feature: fsharp mode expandsions
  In order to quickly and precisely mark F# code blocks
  As an Emacs user
  I want to expand to them

  Background:
    Given there is no region selected
    And I turn on fsharp-mode
 
  Scenario: Baseline feature test.
    When I insert "answer 42"
    And I place the cursor between "r" and " "
    And I press "C-@"
    And I press "C-@"
    Then the region should be "answer 42"

  Scenario: Mark block
    When I insert:
      """
      if true then
          1
      else
          2
      """
    And I place the cursor before "if"
    And I press "C-@"
    Then the region should be:
      """
      if
      """
    And I press "C-@"
    Then the region should be:
      """
      if true then
          1
      """
    And I press "C-@"
    Then the region should be:
      """
      if true then
          1
      else
          2
      """

  Scenario: Mark inside string
    When I insert ""Douglas Adams""
    And I place the cursor between "A" and "d"
    And I press "C-@"
    And I press "C-@"
    Then the region should be "Douglas Adams"

  # TODO Scenario: Mark inside multi-line string
  # TODO Scenario: Mark nested block
  # TODO Scenario: Mark nested function call
  # TODO Scenario: Mark match block


  
