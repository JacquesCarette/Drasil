#!/usr/bin/python3
import json
import os
import subprocess
import sys

# Assuming Python 3.5 (Xenial only has up to 3.5).
# Can't use fstrings

def is_ev_undefined(l):
  e = os.environ.get(l, None)
  return e is None or not e

if is_ev_undefined("LABEL_FILE"):
  print("Missing temp file location.")
  sys.exit(1)

if is_ev_undefined("TRAVIS_PULL_REQUEST_BRANCH"):
  print("Not a pull request.")
  print("Not running post-build step.")
  sys.exit(0)

if is_ev_undefined("BOT_TOKEN"):
  print("Secure variables unavailable.")
  print("PR from forked repo?")
  print("Skipping.")
  sys.exit(0)

LABEL_FILE = os.environ["LABEL_FILE"]
BOT_TOKEN = os.environ["BOT_TOKEN"]
TRAVIS_REPO_SLUG = os.environ["TRAVIS_REPO_SLUG"]
TRAVIS_PULL_REQUEST = os.environ["TRAVIS_PULL_REQUEST"]


labels = []
if os.path.exists(LABEL_FILE):
  with open(LABEL_FILE, "r") as f:
    labels = list(map(lambda l: l[:-1], f))

labels = set(labels)

c = subprocess.run(["curl", "--max-time", "5", "-f", "-s", "-S",
  "-X", "PUT", "-d", json.dumps({"labels": list(labels)}), "-H",
  "Authorization: token {}".format(BOT_TOKEN),
  "https://api.github.com/repos/{0}/issues/{1}/labels".format(TRAVIS_REPO_SLUG, TRAVIS_PULL_REQUEST)])

sys.exit(c.returncode)
