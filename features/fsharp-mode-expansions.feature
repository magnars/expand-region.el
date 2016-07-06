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

  Scenario: Mark nested block
    When I insert:
      """
      let thing =
          let item = func arg
      """
    And I place the cursor before "item"
    And I press "C-@"
    And I press "C-@"
    Then the region should be:
      """
          let item = func arg
      """

  Scenario: Mark multi-line string
    When I insert:
      """
      let one = 1

      "This is a multi-line FSharp string
      with lots of useless content.
      "
      
      let two = 2
      """
    And I place the cursor between "-" and "l"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    Then the region should be:
      """
      This is a multi-line FSharp string
      with lots of useless content.

      """
    
  Scenario: Mark long symbol name
    When I insert "let ``this is long`` = 1 + 2"
    And I place the cursor between "t" and "h"
    And I press "C-@"
    And I press "C-@"
    Then the region should be "this is long"
  

  
