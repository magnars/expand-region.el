Feature: haskell-mode expansions
  In order to quickly and precisely mark haskell code blocks
  As an Emacs user
  I want to expand to them

  Scenario: Mark instance variable
    Given I turn on haskell-mode
    When I insert:
    """
    main :: IO ()
    main = return ()
    """
    And I place the cursor before "return"
    And I press "C-@"
    And I press "C-@"
    Then the region should be
    """
    main :: IO ()
    main = return ()
    """

