# Setting up an MTA for Belenios

To be fully operational, Belenios needs a working mail transfer agent
(MTA). Here, we assume that:
- Belenios has been compiled and deployed as described
  [here](nspawn/README.md)
- a reverse-proxy has been set up as described
  [here](reverse-proxy.md)
- the deployed server has direct access to Internet. In particular,
  SMTP ports (25, 465) are not filtered.

## Basic setup

On a Debian 12 system, as root:
```
apt install exim4-daemon-light bsd-mailx dns-root-data
```

Configure Exim as an Internet site:
```
dpkg-reconfigure -plow exim4-config
```
and select `Internet site`.

## Recommended setup

To avoid mails sent by Belenios being considered as spam, we recommend
setting up:
- [SPF](https://en.wikipedia.org/wiki/Sender_Policy_Framework)
- [DKIM](https://en.wikipedia.org/wiki/DomainKeys_Identified_Mail)
- [DMARC](https://en.wikipedia.org/wiki/DMARC)

This should be done in the context of a general mailing policy, which
is outside of the scope of this document.
