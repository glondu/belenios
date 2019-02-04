Scenario 2: Vote with vote codes in manual mode and manual trustees
=================================

## Introduction and parameters

Protagonists to emulate: election administrator, credential authority, 2 trustees, `K` electors, an auditor.

Administrator and trustees uses only thier browser. Credential authority uses her browser and sends emails.

Electors use their browser and read emails sent by the server and by the credential authority.

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

## Detailed steps of the Test Scenario 1 process

- Starting setup of the election (action of the administrator)
    - Creation of the draft election
        - Alice has been given administrator rights on an online voting app called Belenios. She goes to check out its homepage and logs in.
        - She clicks on the "Prepare a new election" link
        - She picks the Credential management method: manual
        - (She keeps default value for Authentication method: it is Password, not CAS)
        - She clicks on the "Proceed" button (this redirects to the "Preparation of election" page)
        - She changes values of fields name and description of the election
        - She clicks on the "Save changes button" (the one that is next to the election description field)
        - She remembers the URL of the draft election administration page
    - Edition of election's questions
        - She clicks on the "Edit questions" link, to write her own questions
        - She arrives on the Questions page. She checks that the page title is correct
        - She removes answer 3
        - She clicks on the "Save changes" button (this redirects to the "Preparation of election" page)
    - Setting election's voters
        - She clicks on the "Edit voters" link, to then type the list of voters
        - She types `N` e-mail addresses (the list of invited voters)
        - She clicks on the "Add" button to submit changes
        - She clicks on "Return to draft page" link
        - In "Authentication" section, she clicks on the "Generate and mail missing passwords" button
        - She checks that the page contains expected confirmation text, instead of an error
        - She clicks on the "Proceed" link
        - In "Credentials" section, she clicks on "Credential management" link
        - She remembers the link displayed
        - She sends the remembered link to the credential authority by email
        - She logs out and closes the browser
- Credential authority sends credentials to electors
    - Cecily, the Credential Authority, receives the email sent by Alice, and opens the link in it
    - She remembers what the link to the election will be, so that she will be able to send it to voters by email with their private credential
    - She clicks on the "Generate" button
    - She clicks on the "private credentials" and "public credentials" links and downloads these files. (Files are by default downloaded using filenames `creds.txt` and `public_creds.txt` respectively)
    - She clicks on the "Submit public credentials" button
    - She checks that redirected page shows correct confirmation sentence
    - She closes the window
    - She reads the private credentials file (`creds.txt`) and sends credential emails to voters
- Continuing setup of the election: Administrator invites trustees
    - Administrator logs in and goes to the election draft page
    - In the trustees section, she clicks on the "here" link
    - She adds two trustees (their email address), and remembers the link she will send to each trustee
    - She sends to each trustee an email containing their own link
    - She logs out and closes the window
- Trustees generate election private keys. Each trustee (Tom and Taylor) will do the following process:
    - Trustee opens link that has been sent to him by election administrator
    - He checks that the page content shows the same election URL as the one the administrator saw
    - He clicks on the "Generate a new keypair" button
    - He clicks on the "private key" and "public key" links, to download the private key and the public key (files are respectively saved by default as `private_key.json` and `public_key.json`)
    - He clicks on the "Submit public key" button
    - He checks that the next page shows the expected confirmation sentence
    - He closes the window
- Administrator completes setup of the election
    - Alice, as an administrator of an election, wants to finalize her draft election creation, to start the vote. She opens a browser and logs in as administrator
    - She goes to the draft election administration page
    - In "Validate creation" section, she clicks on the "Create election" link
    - (She arrives on the "Checklist" page, that lists all main parameters of the election for review, and that flags incoherent or misconfigured parameters. For example, in this test scenario, it displays 2 warnings: "Warning: No trustees were set. This means that the server will manage the election key by itself.", and "Warning: No contact was set!")
    - In the "Validate creation" section, she clicks on the "Create election" button
    - (She arrives back on the "My test election for Scenario 1 — Administration" page. Its contents have changed. There is now a text saying "The election is open. Voters can vote.", and there are now buttons "Close election", "Archive election", "Delete election")
    - She remembers the URL of the voting page, that is where the "Election home" link points to
    - She checks that a "Close election" button is present (but she does not click on it)
    - She logs out and closes the window
- Verify election consistency (using command line tool `belenios_tool verify`)
- All voting electors cast their vote (`K` electors vote). We check vote data consistency for every batch of `X` votes (using `belenios_tool verify-diff` and a snapshot of election data copied in previous batch). For each batch of `X` voters:
    - Create election data snapshot
    - Current batch of electors vote. For each voter of this batch:
        - Bob checks that he has received 2 emails containing an invitation to vote and all necessary credentials (election page URL, username, password). He goes to the election page URL.
        - He clicks on the "Start" button
        - A loading screen appears, then another screen appears. He clicks on the "Here" button
        - A modal opens (it is an HTML modal created using Window.prompt()), with an input field. He types his credential.
        - He fills his votes to each answer of the question (for each displayed checkbox, he decides to mark it or leave it empty)
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
- Administrator starts tallying of the election
    - Alice goes to the election page
    - She clicks on the "Administer this election" link
    - She logs in as administrator
    - She clicks on the "Close election" button
    - She clicks on the "Proceed to vote counting" button
    - She remembers the encrypted tally hash
    - She remembers the link to send to each trustee, so they can tally the election
    - She sends to each trustee an email containing their own link
    - She logs out and closes the window
- Trustees do tallying (partial decryption). Each trustee (Tom and Taylor) will do the following process:
    - He opens the link that Alice (the election administrator) has sent to him
    - We verify that the encrypted election hash is the same as the one that has been displayed to election administrator
    - He verifies that the "private key" input field is empty (at the beginning)
    - He clicks on the "Browse..." button and selects his private key file (initially downloaded as `private_key.json` by default)
    - He waits until the "private key" input field (that has id "#private_key") becomes not empty anymore. This is because once the user has selected the file to upload, the Javascript code in the page detects that a file has been selected, reads it, and fills "private key" input field with file's contents. The computation triggered by click on the "Compute decryption factors" button will use the value of this field, not directly the uploaded file contents.
    - He clicks on the "Compute decryption factors" button
    - He checks that the text field below (used as visual feedback) now contains text
    - He clicks on the "Submit" button
    - He checks that next screen contains a confirmation sentence
    - He closes the window
- Administrator finished tallying of the election
    - Alice goes to the election page
    - She clicks on the "Administer this election" link
    - She logs in as administrator
    - She checks that encrypted tally hash is still the same as the first time it has been displayed to her
    - She checks that the "DONE?" column of each trustee is to "Yes"
    - She clicks on the "Compute the result" button
    - She checks consistency of the election result
        - She checks that the number of accepted ballots is the same as the number of voters who voted
        - For each available answer in the question, she checks that the total number of votes in favor of Answer X displayed in result page is the same as the sum of votes for Answer X in all votes of voters who voted that have been randomly generated in advance
        - She checks that each ballot content corresponds to content that of this vote that has been randomly generated in advance
- Verify election consistency (using command line tool `belenios_tool verify`)

