#!/usr/bin/python
# coding: utf-8
import re
import subprocess
import tempfile


class FakeSentEmailsManager:
    def __init__(self, log_file_path=None):
        if log_file_path is None:
            (file_handle, log_file_path) = tempfile.mkstemp(text=True)
        self.log_file_path = log_file_path
        # self.install_fake_sendmail_log_file()


    def find_in_sent_emails(self, text):
        with open(self.log_file_path) as fl:
            return text in fl.read()


    def count_occurences_in_sent_emails(self, text):
        with open(self.log_file_path) as file:
            count = file.read().count(text)
        return count


    def separate_sent_emails(self):
        """
        Converts the file that gathers all sent emails to an array with one element per sent email. Each element is a dictionary with fields "to", "subject", and "full_content".
        :return: array
        """

        # Email content is encoded using "quoted-printable" encoding. Please refer to https://en.wikipedia.org/wiki/Quoted-printable for more information. For example, this enconding transforms "@" into "=40". TODO: We could improve this function by having it directly decode the part of the email that is encoded, using `quopri` library for example.
        marker_for_end_of_email = "--=20"
        result = []
        with open(self.log_file_path) as file:
            contents = file.read()
            separated_emails = contents.split(marker_for_end_of_email)

            if len(separated_emails[-1]) < 5: # The last sent email ends with marker_for_end_of_email, so we can ignore what comes after
                separated_emails.pop()

            for email_full_content in separated_emails:
                email_to = ""
                match = re.search(r'^To: "(.*)"', email_full_content, re.MULTILINE)
                if match:
                    email_to = match.group(1)

                email_subject = ""
                match = re.search(r'^Subject: (.*)$', email_full_content, re.MULTILINE)
                if match:
                    email_subject = match.group(1)

                element = {
                    "to": email_to,
                    "subject": email_subject,
                    "full_content": email_full_content
                }
                result.append(element)
        return result


    def install_fake_sendmail_log_file(self):
        subprocess.run(["rm", "-f", self.log_file_path]) # TODO: Execute a command that works on other OS, like `os.remove()`
        subprocess.run(["touch", self.log_file_path]) # TODO: Execute a command that works on other OS, like `pathlib.Path.touch()`


    def uninstall_fake_sendmail_log_file(self):
        subprocess.run(["rm", "-f", self.log_file_path]) # TODO: Execute a command that works on other OS, like `os.remove()`


    def send_email(self, from_email_address, to_email_address, subject, content):
        from datetime import datetime
        username_and_email_format = "\"{username}\" <{email_address}>"
        from_label = username_and_email_format.format(username=from_email_address, email_address=from_email_address)
        to_label = username_and_email_format.format(username=to_email_address, email_address=to_email_address)
        date_label = datetime.now().strftime("%a, %d %b %Y %H:%M:%S %z")
        full_content_format = """\
Content-type: text/plain; charset="UTF-8"
Content-transfer-encoding: quoted-printable
From: {from_label}
To: {to_label}
Subject: {subject}
MIME-Version: 1.0
X-Mailer: Belenios Automated Tests
Date: {date_label}

{content}

--=20\
"""
        full_content = full_content_format.format(from_label=from_label, to_label=to_label, subject=subject, date_label=date_label, content=content)
        with open(self.log_file_path, "a") as myfile:
            myfile.write(full_content)
