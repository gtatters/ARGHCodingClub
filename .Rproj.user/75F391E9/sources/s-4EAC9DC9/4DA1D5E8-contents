# open shell from the git "more" icon and type the following:
# git add -A && git commit -m 'staging all files'

# then push any changes you've made


commitpush<-function(txt="'staging all files'"){
  #cmd<-paste0("git add -A && git commit -m", " ", txt)
  
  system2(command="git", args="add -A" )
  Sys.sleep(2)  
  
  system2(command="git", args=paste0("commit -m", " ", txt))
  Sys.sleep(2)
  system2(command="git", args="push")

  }

commitpush(txt="'staging all files'")


.Rproj.user
.Rhistory
.RData
.Ruserdata
.gitignore
commands.R
ARGHCodingClub.Rproj
