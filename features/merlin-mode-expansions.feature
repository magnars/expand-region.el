Feature: merlin-mode expansions
  In order to quickly and precisely mark merlin code blocks
  As an Emacs user
  I want to expand to them

  Scenario: merlin mark name
    Given I turn on merlin-mode
    And there is no region selected
    When I insert:
    """
    let () =
      List.iter (fun s -> print_endline s) ["foo"; "bar"]
    """
    ;; why does the below line not fail?
    And x
    And I place the cursor before "endline"
    And I expand the region
    Then the region should be "print_endline"
    And I expand the region
    Then the region should be "print_endline s"
    And I expand the region
    Then the region should be "s -> print_endline s"
    And I expand the region
    Then the region should be "(fun s -> print_endline s)"
    And I expand the region
    Then the region should be:
    """
    List.iter (fun s -> print_endline s) ["foo"; "bar"]
    """
    And I expand the region
    Then the region should be:
    """
    let () =
      List.iter (fun s -> print_endline s) ["foo"; "bar"]
    """

  Scenario: merlin mark fun
    Given I turn on merlin-mode
    And there is no region selected
    When I insert:
    """
    let () =
      List.iter (fun s -> print_endline s) ["foo"; "bar"]
    """
    And I place the cursor before "fun "
    And I expand the region
    Then the region should be:
    """
    (fun s -> print_endline s)
    """
    And I expand the region
    Then the region should be:
    """
    List.iter (fun s -> print_endline s) ["foo"; "bar"]
    """