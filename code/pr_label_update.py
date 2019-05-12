#!/usr/bin/python3
import contextlib
import json
import os
import subprocess
import sys

# Assuming Python 3.5 (Xenial only has up to 3.5).
# Can't use fstrings

def is_ev_undefined(l):
  e = os.environ.get(l, None)
  return e is None or not e

if is_ev_undefined("LABEL_FILE") or is_ev_undefined("MANAGED_LABEL_FILE"):
  print("Missing label file (possibly) created from `ci_fstep`.")
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
MANAGED_LABEL_FILE = os.environ["MANAGED_LABEL_FILE"]
BOT_TOKEN = os.environ["BOT_TOKEN"]
TRAVIS_REPO_SLUG = os.environ["TRAVIS_REPO_SLUG"]
TRAVIS_PULL_REQUEST = os.environ["TRAVIS_PULL_REQUEST"]

def wrap_terminal_yellow(s):
  return "\033[0K\033[33;1m{}\033[0m".format(s)

@contextlib.contextmanager
def travis_fold(section_ident, section_title):
  print("travis_fold:start:{0}{1}".format(section_ident, wrap_terminal_yellow(section_title)))
  try:
    yield
  finally:
    print()
    print("travis_fold:end:{}".format(section_ident))
    print("\033[0K")

class RetriesExpired(Exception):
  pass

def retry_subproc(c, success, attempts=3):
  for _ in range(attempts):
    r  = c()
    if success(r):
      return r
  raise RetriesExpired()

def retry_curl(args):
  return retry_subproc(lambda: subprocess.run(["curl", "--max-time", "5", "-f",
    "-s", "-S"] + args, stdout=subprocess.PIPE, universal_newlines=True),
    lambda c: c.returncode == 0)

def read_labels(fn):
  labels = []
  if os.path.exists(fn):
    with open(fn, "r") as f:
      labels = list(map(lambda l: l[:-1], f))
  return set(labels)

def make_mutation_request(labelable_id, labels):
  return """mutation {{
  removeLabelsFromLabelable(input:{{labelableId:"{0}", labelIds:[{1}]}}) {{
    clientMutationId
  }}
}}""".format(labelable_id, ",".join(map(lambda l: '"{}"'.format(l), labels)))

if __name__ == "__main__":
  labels = read_labels(LABEL_FILE)
  managed_labels = read_labels(MANAGED_LABEL_FILE)

  with travis_fold("debug_api", "GitHub API Responses"):
    pr = retry_curl(["-H", "Authorization: token {}".format(BOT_TOKEN),
      "https://api.github.com/repos/{0}/pulls/{1}".format(TRAVIS_REPO_SLUG, TRAVIS_PULL_REQUEST)])

    print(pr.stdout)

    pr_blob = json.loads(pr.stdout)
    pr_labels = {l["name"]: l["node_id"] for l in pr_blob["labels"]}

    managed_labels_on_pr = managed_labels & set(pr_labels.keys());
    labels_to_remove = managed_labels_on_pr - labels
    labels_to_add = labels - managed_labels_on_pr

    if labels_to_remove:
      mutation = make_mutation_request(pr_blob["node_id"], list(map(lambda l: pr_labels[l], labels_to_remove)))
      mr = retry_curl(["-X", "POST", "-d", json.dumps({"query": mutation}),
        "-H", "Authorization: bearer {}".format(BOT_TOKEN), "https://api.github.com/graphql"])
      print(mr.stdout)

    if labels_to_add:
      c = retry_curl(["-X", "POST", "-d", json.dumps({"labels": list(labels_to_add)}), "-H",
          "Authorization: token {}".format(BOT_TOKEN),
          "https://api.github.com/repos/{0}/issues/{1}/labels".format(TRAVIS_REPO_SLUG, TRAVIS_PULL_REQUEST)])
      print(c.stdout)
