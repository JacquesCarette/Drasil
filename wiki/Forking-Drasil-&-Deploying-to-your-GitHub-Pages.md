If you would like to fork Drasil and use our default method (GitHub Actions) of builds and deployments to GitHub Pages, you will need to:

1. Enable "Actions" on your forked repository (under the Settings section of your repository).
2. Create an API-ready GitHub account to act as the one performing updates to `gh-pages`.
3. Create a Personal Access Token for your GitHub account. Please use this picture as a guide for which permissions you will need to assign your PAT: ![see PR: https://github.com/JacquesCarette/Drasil/pull/2461](https://user-images.githubusercontent.com/1627302/118285995-036a5780-b4a0-11eb-83c3-d0afeefe8f20.png)
4. Configure 3 repository secrets for your Drasil fork:
   1. `BOT_TOKEN` containing the personal access token of your API-ready account.
   2. `BOT_EMAIL` containing the email of the bot (email has to be on the list of emails registered on the bots account).
   3. `BOT_NAME` containing the name (e.g., "drasil-bot") of the bot.
5. Test your bot and the Actions configuration on your fork: make and merge a PR, or create a commit with "[workflow-trigger]" anywhere in the commit message.
