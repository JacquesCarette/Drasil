# AI Policy

## 1. Restrictions

1. **Core codebase (`code/drasil-*`)**: It is **forbidden** to use any Artificial Intelligence (AI), such as Large Language Models (e.g., Copilot, ChatGPT, Claude, Gemini). All contributions within these paths must be authored by humans.
2. **Supporting Infrastructure**: AI assistance is permitted for any file outside of the core codebase. For example, you may use them build scripts, configuring linters, your personal notes, etc.

## 2. Acknowledgment

Should you use AI in any capacity, you **must include acknowledgment** in your commit messages and pull request (PR) descriptions.

### 2.1. Commit Messages

Use [git commit trailers](https://git-scm.com/docs/git-commit#Documentation/git-commit.txt---trailertokenvalue) to indicate which AI tool you used. Some recommendations:

1. ChatGPT: `Co-authored-by: ChatGPT <chatgpt@openai.com>`
2. Claude: `Co-authored-by: Claude <claude@anthropic.com>`
3. Copilot: `Co-authored-by: Copilot <copilot@github.com>`
3. Gemini: `Co-authored-by: Gemini <gemini@google.com>`

If you author your commit messages through terminal, you should modify your normal `git commit` command as follows:

```shell
git commit -m "<my normal commit message>" --trailer "Co-authored-by: MyAITool <noreply@example.com>"
```

### 2.2. Pull Request Descriptions

In the PR description, please describe the capacity to which you used AI along with any information relevant to your usage (e.g., which LLMs? Prompts? etc.).
