Feature: Expand Region
  In order to quickly and precisely mark units
  As an Emacs user
  I want to expand to them

  Scenario: Mark entire word with point midword
    Given there is no region selected
    When I insert "This is some text"
    And I go to point "10"
    And I expand the region
    Then the region should be "some"

  Scenario: Mark word just behind point
    Given there is no region selected
    When I insert "This is some text"
    And I go to point "13"
    And I expand the region
    Then the region should be "some"

  Scenario: Multiple expand-region
    Given there is no region selected
    When I insert "This (is some) text"
    And I go to point "10"
    And I expand the region 3 times
    Then the region should be "(is some)"

  Scenario: Expand from existing selection
    Given there is no region selected
    When I insert "This (is some) text"
    And I go to point "7"
    And I set the mark
    And I go to point "14"
    And I expand the region
    Then the region should be "(is some)"

  Scenario: Skip white space forward if spaces on both sides of cursor
    Given there is no region selected
    When I insert "This is    some text"
    And I go to point "10"
    And I expand the region
    Then the region should be "some"

#  Scenario: Skip white space forward if at beginning of buffer
#    Given there is no region selected
#    When I insert "   This is some text"
#    And I go to point "1"
#    And I expand the region
#    Then the region should be "This"

  Scenario: Skip white space forward if at beginning of line
    Given there is no region selected
    When I insert:
    """
    This is
       some text
    """
    And I go to point "9"
    And I expand the region
    Then the region should be "some"
