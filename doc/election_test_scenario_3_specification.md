Scenario 3: Simple vote, in "fully automated" mode, with non-homomorphic question
=================================

## Introduction and parameters

Protagonists to emulate: election administrator, `K` electors, an auditor.

Administrator uses only her browser.

Electors use their browser and read emails sent by the server.

`L` electors re-vote (with `L <= K`)

`M` electors ask administrator to re-generate their password, and vote with their re-generated password (with `M <= K`).

The auditor makes web requests, has a persistent state, and runs the commandline version of the Belenios tool.

Auditor makes web requests, has a persistent state, and runs the commandline version of the Belenios tool.
Authentication of administrator and electors are done using a login / password combination.

Examples of parameters sizes: `N` and `K` would be between 6 (quick test) and 1000 (load test)

## Note about verification

Verifications all along the process is done using command line tools `belenios-tool verify` and `belenios-tool verify-diff`:

    - `belenios-tool verify` does a static verification (it verifies that vote data at current time is coherent)
    - `belenios-tool verify-diff` does a dynamic verification (it verifies that current state of vote data is a possible/legitimate evolution of a vote data snapshot that has been saved during a previous step of the process)

## Detailed steps of the Test Scenario 3 process

- Starting setup of the election (action of the administrator)
    - Creation of the draft election
        - Alice has been given administrator rights on an online voting app called Belenios. She goes to check out its homepage and logs in.
        - She clicks on the "Prepare a new election" link
        - (She keeps default values on the form: Credential management is automatic (not manual), and Authentication method is Password, not CAS)
        - She clicks on the "Proceed" button (this redirects to the "Preparation of election" page)
        - She changes values of fields name and description of the election
        - She clicks on the "Save changes button" (the one that is next to the election description field)
    - Edition of election's questions
        - She clicks on the "Edit questions" link, to write her own questions
        - She arrives on the Questions page. She checks that the page title is correct
        - She clicks on "Tick the box to activate this mode."
        - She clicks on "Alternative"
        - She removes answer 3
        - She clicks on the "Save changes" button (this redirects to the "Preparation of election" page)
    - Setting election's voters
        - She clicks on the "Edit voters" link, to then type the list of voters
        - She types `N` e-mail addresses (the list of invited voters)
        - She clicks on the "Add" button to submit changes
        - She clicks on "Return to draft page" link
    - She clicks on button "Generate on server"
    - (Server sends emails to voters.) She checks that server does not show any error that would happen when trying to send these emails (this can happen if sendmail is not configured)
    - We do a sanity check that server has really tried to send emails. (For this, we look for email addresses in the temporary file where our fake sendmail executable redirects its inputs to.)
    - She clicks on the "Proceed" link
    - In "Authentication" section, she clicks on the "Generate and mail missing passwords" button
    - She checks that the page contains expected confirmation text, instead of an error
    - She clicks on the "Proceed" link
    - Finalize creation of election
        - In "Validate creation" section, she clicks on the "Create election" link
        - (She arrives on the "Checklist" page, that lists all main parameters of the election for review, and that flags incoherent or misconfigured parameters. For example, in this test scenario, it displays 2 warnings: "Warning: No trustees were set. This means that the server will manage the election key by itself.", and "Warning: No contact was set!")
        - In the "Validate creation" section, she clicks on the "Create election" button
        - (She arrives back on the "My test election for Scenario 1 — Administration" page. Its contents have changed. There is now a text saying "The election is open. Voters can vote.", and there are now buttons "Close election", "Archive election", "Delete election")
        - She remembers the URL of the voting page, that is where the "Election home" link points to
        - She checks that a "Close election" button is present (but she does not click on it)
    - Log out and close the browser window
- Regenerating electors' lost passwords (for M electors) (action of the administrator)
    - Alice has been contacted by some voters who say they lost their password. She wants to re-generate their passwords and have the platform send them by email. For this, she logs in as administrator.
    - She remembers the list of voters who contacted her and said they lost their password.
    - She selects the election that she wants to edit
    - She arrives to the election administration page. For each voter of the M selected voters:
        - She clicks on the "Regenerate and mail a password" link
        - She types the e-mail address of the voter in the "Username" field
        - She clicks on the "Submit" button
        - She checks that the page shows a confirmation message similar to "A new password has been mailed to name@email.com"
        - She clicks on the "Proceed" link (She arrives back to the election administration page)
        - We do a sanity check that server has really tried to send these emails, and to these users only.
    - She logs out and closes the browser window
- Verify election consistency (using command line tool `belenios_tool verify`)
- All voting electors cast their vote (`K` electors vote). We check vote data consistency for every batch of `X` votes (using `belenios_tool verify-diff` and a snapshot of election data copied in previous batch). For each batch of `X` voters:
    - Create election data snapshot
    - Current batch of electors vote. For each voter of this batch:
        - Bob checks that he has received 2 emails containing an invitation to vote and all necessary credentials (election page URL, username, password). He goes to the election page URL.
        - He clicks on the "Start" button
        - A loading screen appears, then another screen appears. He clicks on the "Here" button
        - A modal opens (it is an HTML modal created using Window.prompt()), with an input field. He types his credential.
        - He fills his votes to each answer of the question (for each displayed input field, he decides to set it to 1 or leave it at zero)
        - He clicks on the "Next" button
        - He remembers the smart ballot tracker that is displayed
        - He clicks on the "Continue" button
        - He types his voter username and password, and submits the form
        - He checks that the smart ballot tracker value that appears on screen is the same as the one he noted
        - He clicks on the "I cast my vote" button
        - He clicks on the "ballot box" link
        - He checks that his smart ballot tracker appears in the list
        - He closes the window (there is no log-out link, because user is not logged in: credentials are not remembered)
        - He checks his mailbox to find a new email with confirmation of his vote, and verifies the value of the smart ballot tracker written in this email is the same as the one he noted.
    - Verify election consistency (using `belenios_tool verify-diff`)
    - Delete election data snapshot
- Verify election consistency (using command line tool `belenios_tool verify`)
- Create election data snapshot
- All electors who want to change their vote re-vote (`L` electors re-vote)
    - We re-apply the same procedure as listed in previous step, except we use the set of `L` re-voters instead of the set of `K` voters
- Verify election consistency (using `belenios_tool verify-diff` and the snapshot created right before re-votes)
- Delete election data snapshot
- Verify election consistency (using command line tool `belenios_tool verify`)
- Administrator does tallying of the election
    - Alice goes to the election page
    - She clicks on the "Administer this election" link
    - She logs in as administrator
    - She clicks on the "Close election" button
    - She clicks on the "Proceed to vote counting" button
    - She clicks on the "Proceed to decryption" button
- Verify election consistency (using command line tool `belenios_tool verify`)
- One voter tries to revote and faces an error message "Your ballot for Test vote after close is rejected, because the election is closed." after authentication
