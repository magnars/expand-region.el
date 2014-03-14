Feature: org-mode expansions
  In order to quickly and precisely mark org mode sections
  As an Emacs user
  I want to expand to them

  Scenario: Org level 3
    Given I turn on org-mode
    When I insert:
    """
    * lvl 1
    ** lvl 2
    *** lvl 3
    """
    And I place the cursor before "*** lvl 3"
    And I press "C-@"
    And I press "C-@"
    Then the region should be "*** lvl 3"

  Scenario: Org level 2
    Given I turn on org-mode
    When I insert:
    """
    * lvl 1
    ** lvl 2
    *** lvl 3
    """
    And I place the cursor before "*** lvl 3"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    Then the region should be:
    """
    ** lvl 2
    *** lvl 3
    """

  Scenario: Org level 1
    Given I turn on org-mode
    When I insert:
    """
    * lvl 1
    ** lvl 2
    *** lvl 3
    """
    And I place the cursor before "*** lvl 3"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    Then the region should be:
    """
    * lvl 1
    ** lvl 2
    *** lvl 3
    """
  Scenario: Org list item
    Given I turn on org-mode
    When I insert:
    """
    * lvl 1
    ** lvl 2
    - list item one
    - list item two
    #+BEGIN_SRC html
    <div class="myclass">Some text</div>
    #+END_SRC

    This is a sentence.  And a second one makes a paragraph.

    *** lvl 3
    """
    And I place the cursor before "item one"
    And I press "C-@"
    And I press "C-@"
    Then the region should be:
    """
    list item one

    """

  Scenario: Org list element
    Given I turn on org-mode
    When I insert:
    """
    * lvl 1
    ** lvl 2

    - list item one
    - list item two

    #+BEGIN_SRC html
    <div class="myclass">Some text</div>
    #+END_SRC

    This is a sentence.  And a second one makes a paragraph.

    *** lvl 3
    """
    And I place the cursor before "item one"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    Then the region should be:
    """
    - list item one

    """

  Scenario: Org entire list
    Given I turn on org-mode
    When I insert:
    """
    * lvl 1
    ** lvl 2

    - list item one
    - list item two

    #+BEGIN_SRC html
    <div class="myclass">Some text</div>
    #+END_SRC

    This is a sentence.  And a second one makes a paragraph.

    *** lvl 3
    """
    And I place the cursor before "item one"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    Then the region should be:
    """
    - list item one
    - list item two
    """

  Scenario: Org from list to subheading
    Given I turn on org-mode
    When I insert:
    """
    * lvl 1
    ** lvl 2
    - list item one
    - list item two

    #+BEGIN_SRC html
    <div class="myclass">Some text</div>
    #+END_SRC

    This is a sentence.  And a second one makes a paragraph.

    *** lvl 3
    """
    And I place the cursor before "item one"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    And I press "C-@"
    Then the region should be:
    """
    ** lvl 2
    - list item one
    - list item two

    #+BEGIN_SRC html
    <div class="myclass">Some text</div>
    #+END_SRC

    This is a sentence.  And a second one makes a paragraph.

    *** lvl 3
    """

  Scenario: Org code block
    Given I turn on org-mode
    When I insert:
    """
    * lvl 1
    ** lvl 2

    - list item one
    - list item two

    #+BEGIN_SRC html
    <div class="myclass"> Some text </div>
    #+END_SRC

    This is a sentence.  And a second one makes a paragraph.

    *** lvl 3
    """
    And I place the cursor between "Some " and "text"
    And I press "C-@"
    And I press "C-@"
    Then the region should be:
    """
    #+BEGIN_SRC html
    <div class="myclass"> Some text </div>
    #+END_SRC
    """
