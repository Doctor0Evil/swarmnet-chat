package egress

deny[msg] {
  input.path == "bitshell.meta.json"
  input.content.trust.blocked_network != true
  msg := "Network egress must be blocked during build/test"
}
