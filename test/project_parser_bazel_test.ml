let expect condition message = if not condition then failwith message

let () =
  expect
    (Project.parse_github_remote "https://github.com/TedKS/discord-agents.git"
     = Some ("tedks", "discord-agents"))
    "HTTPS remote parsing";
  expect
    (Project.parse_github_remote "git@github.com:TedKS/discord-agents.git"
     = Some ("tedks", "discord-agents"))
    "SSH remote parsing";
  expect
    (Project.same_remote_url
       "ssh://git@github.com/TedKS/discord-agents.git"
       "https://github.com/tedks/discord-agents")
    "equivalent remotes should deduplicate";
  expect
    (Project.parse_github_remote "https://github.com/../escape.git" = None)
    "path traversal owner accepted";
  expect
    (match Project.validate_github_url "https://github.com/a/repo name" with
     | Error _ -> true
     | Ok _ -> false)
    "space in GitHub URL accepted";
  expect
    (match Project.validate_import_name "../outside" with
     | Error _ -> true
     | Ok _ -> false)
    "path traversal import name accepted"
