Feature: treesit expansions
  Background:
    Given there is no region selected
    And I turn on rust-ts-mode

    Scenario: Successively expand the region
    When I insert "fn some_func(arg: String) -> String { arg.to_lowercase() }"
    And I place the cursor before "to_lowercase"
    And I press "C-@"
    Then the region should be "to_lowercase"
    And I press "C-@"
    Then the region should be "arg.to_lowercase()"
    And I press "C-@"
    Then the region should be "{ arg.to_lowercase() }"
