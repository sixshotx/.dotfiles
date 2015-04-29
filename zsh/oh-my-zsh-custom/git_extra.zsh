# Do it this way rather than aliasing hub as git b/c it wipes out branch name completion
function git(){hub $@}
# set upstream on push.
alias gsl="git stash list"
alias gwip='git add -A; git ls-files --deleted -z | xargs -0 git rm; git commit -n -m "--wip--"'
alias gunwip='git log -n 1 | grep -q -c "\-\-wip\-\-" && git reset HEAD~1'
alias gmm='branch=$(current_branch); gco master && ggpull && gco $branch && gm master'
function gcb() {
    new_branch=$(gb | fzf)
    no_spaces=$(echo $new_branch | tr -d ' ')
    gco $no_spaces
}

# fbr - checkout git branch
fbr() {
  local branches branch
  branches=$(git branch) &&
  branch=$(echo "$branches" | fzf +m) &&
  git checkout $(echo "$branch" | sed "s/.* //")
}

# fco - checkout git commit
fco() {
  local commits commit
  commits=$(git log --pretty=oneline --abbrev-commit --reverse) &&
  commit=$(echo "$commits" | fzf --tac +s +m -e) &&
  git checkout $(echo "$commit" | sed "s/ .*//")
}

# fshow - git commit browser
fshow() {
  local out sha q
  while out=$(
      git log --decorate=short --graph --oneline --color=always |
      fzf --ansi --multi --no-sort --reverse --query="$q" --print-query); do
    q=$(head -1 <<< "$out")
    while read sha; do
      [ -n "$sha" ] && git show --color=always $sha | less -R
    done < <(sed '1d;s/^[^a-z0-9]*//;/^$/d' <<< "$out" | awk '{print $1}')
  done
}

# delete a git branch
function gbd() {
    branch_to_delete=$(gb | fzf)
    no_spaces=$(echo $branch_to_delete | tr -d ' ')
    gb -d $no_spaces
}

# git branch delete force
function gbdf() {
    branch_to_delete=$(gb | fzf)
    no_spaces=$(echo $branch_to_delete | tr -d ' ')
    gb -D $no_spaces
}

# Git check
# 10 lines of context for diff
alias gch="git diff -U10 master"

# Git "me." My progress in log form
alias gme="git lg master..HEAD"

# git today
# All the stuff I've done today
alias gtoday='git lg --branches --author="jasoncyu" --after="6am"'

# Prettier git diff
alias gd='git diff -U10 --color-words'

# pull-request
# Relies on hub
function gpr-wrapper() {
    return $1
}

function gpr() {
    ggpush -u
    BROWSER=gpr-wrapper g pull-request $@
    echo $BROWSER
    # ehhh a lot of yak shaving to fix this
    # open $BROWSER
}

# new
# creates a new branch off master
function gnew() {
    gco master
    gco -b $1
}

# Reverting merges
# git revert e2f24a4 -m 1 -Xtheirs
# git revert $hash -m $master_hash -Xtheirs
# So when you're reverting a merge, the direction's the opposite of the original.
# THe old commit merged from feature into master, so the revert is like a "ghost"
# merge from master into feature. The point is, the direction is such that "ours"
# is the feature and "theirs" is the master. So we take theirs for any conflict.
#

# Deletes all merged branches
function gdd() {
    git branch --merged | grep -v '^* master$' | grep -v '^  master$' | xargs -n 1 git branch -d
}

# generate patch for use with git apply
# function gpatch() {
    # git diff --color=never --patch-with-raw d31232..d31232^ > remove_shippo.diff
# }

# Show conficted files only
alias gdu="git diff --name-only --diff-filter=U"
# Git resolve conflict. Add a file which is resolved.
function grc() {
    resolved_file=$(gdu | fzf)
    no_spaces=$(echo $resolved_file | tr -d ' ')
    ga $no_spaces
}
# Jump to Pull request on github
alias gcmp="hub compare"
alias pulls="open https://github.com/twiceclothes/twice-web-root/pulls/jasoncyu"

# even faster pushing
alias gp=ggpush
