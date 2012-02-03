Feature: Expand Region
  In order to quickly and precisely select semantic units
  As an Emacs user
  I want to expand to it

  Scenario: Mark entire word with point midword
    Given transient mark mode is active
    And there is no region selected
    When I insert "This is some text"
    And I go to point "10"
    And I expand the region
    Then the region should be "some"

  Scenario: Mark entire word with point just after
    Given transient mark mode is active
    And there is no region selected
    When I insert "This is some text"
    And I go to point "13"
    And I expand the region
    Then the region should be "some"
