package auth

deny[msg] {
  input.path == "bitshell.meta.json"
  input.content.deploy.allow_publish_profile != false
  msg := "Publish profiles are disabled by policy; use Azure OIDC/RBAC"
}
