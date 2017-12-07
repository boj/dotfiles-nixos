set -x PATH $PATH $HOME/.local/bin

set -x LC_ALL en_US.UTF-8
set -x LANG en_US.UTF-8

set -x EDITOR kak

set -x FZF_DEFAULT_COMMAND 'fd --type f'

function fzfe
  fzf | read -l r ; and kak $r
end

if not pgrep -x gpg-agent > /dev/null
  gpg-agent --homedir $HOME/.gnupg --daemon # --enable-ssh-support
end

set -u SSH_AGENT_PID
set -x GPG_TTY (tty)
set -x SSH_AUTH_SOCK (gpgconf --list-dirs agent-ssh-socket)
gpg-connect-agent updatestartuptty /bye > /dev/null

set fish_greeting

if status --is-interactive
  cd
  neofetch
end
