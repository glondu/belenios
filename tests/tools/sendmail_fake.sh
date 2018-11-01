#!/bin/bash
exec >> /tmp/sendmail_fake
while IFS= read -r line; do
  printf '%s\n' "$line"
done