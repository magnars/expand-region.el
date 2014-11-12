Feature: merlin-mode expansions
  In order to quickly and precisely mark merlin code blocks
  As an Emacs user
  I want to expand to them

  Scenario: Mark name
    Given I turn on merlin-mode
    When I insert:
    """
    let () =
      List.iter (fun s ->
          print_endline s)
        ["okta"; "guokte"]
    """
    And I place the cursor before "print_endline"
    And I press "C-@"
    Then the region should be "@print_endline"

  Scenario: Mark fun
    Given I turn on merlin-mode
    When I insert:
    """
    let () =
      List.iter (fun s ->
          print_endline s)
        ["okta"; "guokte"]
    """
    And I place the cursor before "fun "
    And I press "C-@"
    Then the region should be:
    """
    (fun s ->
          print_endline s)
    """
